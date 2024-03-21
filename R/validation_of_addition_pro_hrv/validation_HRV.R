library(dplyr)
library(purrr)
library(tidyr)
library(here)
library(vroom)
source(here::here())

file_path <- here("data-raw/rr-interval/") # Adjust this path
file_pattern <- "*.txt"

# Generate a list of file paths
file_names <- list.files(path = file_path, pattern = file_pattern, full.names = TRUE)
file_names <- file_names[1:(length(file_names) - 2)]

HRV_validation <- function(file_path) {
# Assuming heartbeat_intervals is your dataframe with Intervals loaded
heartbeat_intervals <- read.table(file_path, header = FALSE) %>%
    rename(Intervals = V1) %>%
    mutate(HeartRate = 60000 / Intervals)

# Calculate cumulative milliseconds to segment data
heartbeat_intervals <- heartbeat_intervals %>%
    mutate(CumulativeMS = cumsum(Intervals)) %>%
    mutate(timepoint = ceiling(CumulativeMS / 30000)) %>%
    group_by(timepoint) %>%
    mutate(
        StartTime = first(CumulativeMS) / 1000 - (Intervals / 1000),
        EndTime = last(CumulativeMS) / 1000
    ) %>%
    ungroup()

# Group by timepoint and summarize
timepoint_summary <- heartbeat_intervals %>%
    group_by(timepoint) %>%
    summarise(
        Mean_HR = mean(HeartRate),
        SD = sd(HeartRate),
        N = n(),
        StartTime = first(StartTime),
        EndTime = first(EndTime)
    ) %>%
    mutate(
        ErrorMargin = qt(0.975, df=N-1) * SD / sqrt(N),
        Lower_HR = Mean_HR - ErrorMargin,
        Upper_HR = Mean_HR + ErrorMargin
    ) %>%
    select(timepoint, Mean_HR, Lower_HR, Upper_HR, StartTime, EndTime)


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

validation_HRV <- complete_HRV_validation_loop(file_names)

vroom_write(validation_HRV, here::here("data/validation_HRV_data.csv"))

validation_HRV %>%
ggplot(aes(gen_SDNN, SDNN)) +
    geom_point()

spearmann_sdnn <- cor.test(validation_HRV$gen_SDNN, validation_HRV$SDNN, method=c("spearman"))
spearmann_sdnn$estimate
names(validation_HRV)

validation_HRV %>%
    ggplot(aes(gen_SDNN, SDNN)) +
    geom_point()

validation_HRV %>%
    ggplot(aes(gen_SDANN, SDANN)) +
    geom_point()

validation_HRV %>%
    ggplot(aes(gen_SDNNIDX, SDNNIDX)) +
    geom_point()

spearmann_SDNNIDX <- cor.test(validation_HRV$gen_SDNNIDX, validation_HRV$SDNNIDX, method=c("spearman"))

validation_HRV %>%
    ggplot(aes(gen_RMSSD, RMSSD)) +
    geom_point()

spearmann_RMSSD <- cor.test(validation_HRV$gen_RMSSD, validation_HRV$RMSSD, method=c("spearman"))

validation_HRV %>%
    ggplot(aes(gen_HRVi, HRVi)) +
    geom_point()

spearmann_HRVi <- cor.test(validation_HRV$gen_HRVi, validation_HRV$HRVi, method=c("spearman"))

validation_HRV %>%
    ggplot(aes(gen_meanHR, meanHR)) +
    geom_point()
