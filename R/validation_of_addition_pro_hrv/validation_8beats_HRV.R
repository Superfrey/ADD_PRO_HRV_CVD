analyze_heartbeat_intervals_with_prediction <- function(file_path) {

    read.table(file_path, header = FALSE) %>%
        rename(Intervals = V1) %>%
        mutate(HeartRate = 60000 / Intervals) %>%
        mutate(CumulativeMS = cumsum(Intervals)) %>%
        mutate(timepoint = ceiling(CumulativeMS / 30000)) %>%
        group_by(timepoint) %>%
        mutate(Count = row_number()) %>%
        ungroup() %>%
        group_by(timepoint) %>%
        mutate(
            Last8 = if_else(Count > max(Count) - 8, HeartRate, NA_real_),
            Mean_HR = mean(Last8, na.rm = TRUE),
            SD = sd(Last8, na.rm = TRUE),
            N = sum(!is.na(Last8))
        ) %>%
        summarise(
            Mean_HR = first(Mean_HR),
            SD = first(SD),
            N = first(N)
        ) %>%
        mutate(
            ErrorMargin = qt(0.975, df=N-1) * SD / sqrt(N),
            Lower_HR = Mean_HR - ErrorMargin,
            Upper_HR = Mean_HR + ErrorMargin
        ) %>%
        select(timepoint, Mean_HR, Lower_HR, Upper_HR)

}

# Assuming 'file_names' contains the path to your files
results_list <- map(file_names, analyze_heartbeat_intervals_with_prediction)


HRV_validation <- function(file_path) {
    # Assuming heartbeat_intervals is your dataframe with Intervals loaded
    heartbeat_intervals <- read.table(file_path, header = FALSE) %>%
        rename(Intervals = V1)

    timepoint_summary <- read.table(file_path, header = FALSE) %>%
        rename(Intervals = V1) %>%
        mutate(HeartRate = 60000 / Intervals) %>%
        mutate(CumulativeMS = cumsum(Intervals)) %>%
        mutate(timepoint = ceiling(CumulativeMS / 30000)) %>%
        group_by(timepoint) %>%
        mutate(Count = row_number()) %>%
        ungroup() %>%
        group_by(timepoint) %>%
        mutate(
            Last8 = if_else(Count > max(Count) - 8, HeartRate, NA_real_),
            Mean_HR = mean(Last8, na.rm = TRUE),
            SD = sd(Last8, na.rm = TRUE),
            N = sum(!is.na(Last8))
        ) %>%
        summarise(
            Mean_HR = first(Mean_HR),
            SD = first(SD),
            N = first(N)
        ) %>%
        mutate(
            ErrorMargin = qt(0.975, df=N-1) * SD / sqrt(N),
            Lower_HR = Mean_HR - ErrorMargin,
            Upper_HR = Mean_HR + ErrorMargin
        ) %>%
        select(timepoint, Mean_HR, Lower_HR, Upper_HR)

    generated_ibi <- calculate_ibi(timepoint_summary)

    gen_HRV <- rhrv_time_domain_week(generated_ibi) %>%
        rename_with(~ paste0("gen_", .), .cols = everything())

    heartbeat_intervals <- heartbeat_intervals %>%
        rename(ibi = Intervals)

    HRV <- rhrv_time_domain_week(heartbeat_intervals)

    HRV_test <- cbind(gen_HRV, HRV)
    HRV_test <- as.data.frame(HRV_test)

    return(HRV_test)

}

complete_HRV_validation_loop <- function(list_file_paths) {

    complete_HRV_validation <- possibly(HRV_validation, otherwise = NA)

    validation_HRV <- list_file_paths %>%
        map(~complete_HRV_validation(.x))

    list_tibble <- tibble(vec = validation_HRV)

    # Filter out vectors that are entirely NA
    clean_list_tibble <- list_tibble %>%
        filter(!map_lgl(vec, ~all(is.na(.x))))

    clean_list <- clean_list_tibble$vec

    validation_HRV_final <- bind_rows(clean_list)
    return(validation_HRV_final)
}

validation_HRV_8beat <- complete_HRV_validation_loop(file_names)

vroom_write(validation_HRV_8beat, here::here("data/validation_HRV_8beat_data.csv"))

validation_HRV_8beat %>%
    ggplot(aes(gen_SDNN, SDNN)) +
    geom_point()

spearmann_sdnn <- cor.test(validation_HRV_8beat$gen_SDNN, validation_HRV_8beat$SDNN, method=c("spearman"))
spearmann_sdnn$estimate
names(validation_HRV_8beat)

validation_HRV_8beat %>%
    ggplot(aes(gen_SDNN, SDNN)) +
    geom_point()

validation_HRV_8beat %>%
    ggplot(aes(gen_SDANN, SDANN)) +
    geom_point()

validation_HRV_8beat %>%
    ggplot(aes(gen_SDNNIDX, SDNNIDX)) +
    geom_point()

spearmann_SDNNIDX <- cor.test(validation_HRV_8beat$gen_SDNNIDX, validation_HRV_8beat$SDNNIDX, method=c("spearman"))

validation_HRV_8beat %>%
    ggplot(aes(gen_RMSSD, RMSSD)) +
    geom_point()

spearmann_RMSSD <- cor.test(validation_HRV_8beat$gen_RMSSD, validation_HRV_8beat$RMSSD, method=c("spearman"))
spearmann_RMSSD$estimate

validation_HRV_8beat %>%
    ggplot(aes(gen_HRVi, HRVi)) +
    geom_point()

