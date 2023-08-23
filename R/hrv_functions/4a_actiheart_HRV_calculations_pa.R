files <- list.files(here("xxxx"), pattern = "*.CSV", full.names = T)
# Loading IBI + time generation functions

source(here::here("R/hrv_function/1_generate_IBI_data.R"))
source(here::here("R/hrv_function/2b_hrv_indices_calculations_functions.R"))
source(here::here("R/hrv_function/sleep_function.R"))
pa_data <- read_dta(here::here("PA_XX.dta"))


#############
# Function for extracting ID number from file name
extract_prono <- function(paths) {
    paths %>%
        fs::path_file() %>%
        stringr::str_extract("#.*#") %>%
        stringr::str_to_lower() %>%
        stringr::str_remove_all("#|pro")
}

###Physical ativity

pa_data2 <- pa_data_preperation(pa_data)
sleep_data <- sleep_function(pa_data2)

## Calculating HRV measures for individual and include id number


vroom_hrv <- function(filepath, pa_data, sleep_data) {
    data <- vroom(filepath)
    pro_no <- extract_prono(filepath)
    pro_no_id <- as.numeric(pro_no)


    ibi <- data %>%
        create_time_day_variables() %>%
        calculate_ibi()


    #Physical activity filter pac (physical activity corrected)
    ibi <- cbind(pro_no, ibi)
    ibi$pro_no <- as.numeric(ibi$pro_no)

    # ibi_life <- pa_data %>%
    #     dplyr::filter(pro_no == pro_no_id) %>%
    #     dplyr::mutate(day_number=day_num) %>%
    #     dplyr::right_join(ibi, by = join_by(pro_no, hour, day_number,datetime))
    #     tidyr::drop_na(ibi)
    #
    # ibi_pa <- ibi_life %>%
    #     tidyr::drop_na(time_frach_MET_under_25) %>%
    #     dplyr::filter(time_frach_MET_under_25 > 0.8)

    time_under_25_MET <- pa_data %>%
        dplyr::filter(pro_no == pro_no_id) %>%
        dplyr::mutate(day_number=day_num) %>%
        tidyr::drop_na(time_frach_MET_under_25) %>%
        dplyr::filter(time_frach_MET_under_25 > 0.8 & time_spend_MET_above_3 < 0.2) %>%
        transmute(pair_day_hour = paste(day_number, hour, sep = "_"))

    ibi_pa <- ibi %>%
        filter(paste(day_number, hour, sep = "_") %in% time_under_25_MET$pair_day_hour)

    # Sleep measurement

    most3hour_sleep <- sleep_data %>%
        dplyr::filter(pro_no == pro_no_id) %>%
        transmute(pair_day_hour = paste(day_number, hour, sep = "_"))

    ibi_most3hour_sleep <- ibi %>%
        filter(paste(day_number, hour, sep = "_") %in% most3hour_sleep$pair_day_hour)


    time_sleep <- pa_data %>%
        tidyr::drop_na(SLEEP) %>%
        dplyr::group_by(day_number) %>%
        dplyr::filter(between(hour, 0,6),
                      SLEEP > 0.7) %>%
        transmute(pair_day_hour = paste(day_number, hour, sep = "_"))

    ibi_sleep <- ibi %>%
        filter(paste(day_number, hour, sep = "_") %in% time_sleep$pair_day_hour)


    # week HRV

    hrv_time_domain_week <- rhrv_time_domain_week(ibi)
    hrv_time_domain_week_pa <- rhrv_time_domain_week(ibi_pa) # exclude physical activity
    hrv_time_domain_week_3hsleep <- rhrv_time_domain_week(ibi_most3hour_sleep) # including best 3 hour sleep

    colnames(hrv_time_domain_week_pa) <- base::paste(colnames(hrv_time_domain_week_pa),"pa", sep = '_')
    colnames(hrv_time_domain_week_3hsleep) <- base::paste(colnames(hrv_time_domain_week_3hsleep),"3hour_sleep", sep = '_')

    hrv_time_domain_week <- cbind(pro_no, hrv_time_domain_week, hrv_time_domain_week_pa, hrv_time_domain_week_3hsleep)

    # day HRV

    hrv_time_domain_day <- rhrv_time_domain_day(ibi)
    hrv_time_domain_day_sleep <- rhrv_time_domain_day(ibi_sleep) # including sleep
    hrv_time_domain_day_3hsleep <- rhrv_time_domain_day(ibi_most3hour_sleep) # including best 3 hour sleep
    hrv_time_domain_day_pa <- rhrv_time_domain_day_pa(ibi_pa) # exclude physical activity


    colnames(hrv_time_domain_day_sleep) <- base::paste(colnames(hrv_time_domain_day_sleep),"sleep_0_6", sep = '_')
    colnames(hrv_time_domain_day_3hsleep) <- base::paste(colnames(hrv_time_domain_day_3hsleep),"3hour_sleep", sep = '_')

    colnames(hrv_time_domain_day_pa) <- base::paste(colnames(hrv_time_domain_day_pa),"pa", sep = '_')

    hrv_time_domain_day <- cbind(pro_no, hrv_time_domain_day)
    hrv_time_domain_day <- hrv_time_domain_day %>%
        dplyr::left_join(hrv_time_domain_day_sleep, by = join_by(day_number == day_number_sleep_0_6, week_day == week_day_sleep_0_6)) %>%
        dplyr::left_join(hrv_time_domain_day_pa, by = join_by(day_number == day_number_pa, week_day == week_day_pa)) %>%
        dplyr::left_join(hrv_time_domain_day_3hsleep, by = join_by(day_number == day_number_3hour_sleep, week_day == week_day_3hour_sleep))


    #circ per day

    hrv_time_domain_circ_day <- rhrv_time_domain_circ_day(ibi)

    hrv_time_domain_circ_day_pa <- rhrv_time_domain_circ_day(ibi_pa) # exclude physical activity
    colnames(hrv_time_domain_circ_day_pa) <- base::paste(colnames(hrv_time_domain_circ_day_pa),"pa", sep = '_')


    hrv_time_domain_circ_day <- cbind(pro_no, hrv_time_domain_circ_day)
    hrv_time_domain_circ_day <- hrv_time_domain_circ_day %>%
        dplyr::left_join(hrv_time_domain_circ_day_pa, by = join_by(day_number == day_number_pa, week_day == week_day_pa, circadian_time_points == circadian_time_points_pa))

    # hour HRV per day

    hrv_time_domain_hour_day <- rhrv_time_domain_hour_day(ibi)

    hrv_time_domain_hour_day_pa <- rhrv_time_domain_hour_day_pa(ibi_pa) # exclude physical activity
    colnames(hrv_time_domain_hour_day_pa) <- base::paste(colnames(hrv_time_domain_hour_day_pa),"pa", sep = '_')

    hrv_time_domain_hour_day <- cbind(pro_no, hrv_time_domain_hour_day)

    hrv_time_domain_hour_day <- hrv_time_domain_hour_day %>%
        dplyr::left_join(hrv_time_domain_hour_day_pa, by = join_by(day_number == day_number_pa, week_day == week_day_pa, hour == hour_pa))


    hrv_data <- list(
        week = hrv_time_domain_week, daily = hrv_time_domain_day, hourly_per_day = hrv_time_domain_hour_day,
        circadian_per_day = hrv_time_domain_circ_day
    )
    return(hrv_data)
}


# test <- vroom_hrv(files[23], pa_data2, sleep_data)


#########################

hrv_id_file <- function(filepath, pa_data, data_sleep) {
    data_hrv <- vroom_hrv(filepath, pa_data, data_sleep)
    id_pro <- extract_prono(filepath)
    pro_no_safe <- paste0("data/hrv/hrv_sleep_pa/", "pro_no_", id_pro, ".rds")

    saveRDS(data_hrv, file = here::here(pro_no_safe))
}

#1077 first completed
#error at 679 ID 11501

# error at file 679 ID 11501 PA_MEANS missing
# stopped at file 895 ID 12049 - Too low on heart rate recordings
#  file 1078 ID 12764 - PA_MEANS missing

files1079_1400 <- map(files[1079:1400], ~hrv_id_file(.x, pa_data2, sleep_data))


## Function looping multiple csv files and row binding each individual into a complete data frame

hrv_loop_folder <- function(filepath, pa_data) {
    plan(multisession, workers = 10)

    furrr::future_map2(filepath, pa_data, hrv_id_file,
                       .options = furrr::furrr_options(seed = TRUE)
    )
}

hrv_data <- hrv_loop_folder(files[1], pa_data2)


# load and combine ID HRV

hrv_files <- list.files(here("data/hrv/"), pattern = "*.rds", full.names = T)


HRV_combine <- function(hrv_path) {
    hrv_df <- map(hrv_path, readRDS)
    neasted_hrv_df <- do.call(Map, c(f = rbind, hrv_df))

    return(neasted_hrv_df)
}


data_hrv_nested <- HRV_combine(hrv_files)

### Data frames

hrv_week_df <- data_hrv_nested$week

hrv_day_df <- data_hrv_nested$daily

hrv_hour_day_df <- data_hrv_nested$hourly_per_day

hrv_circadian_day_df <- data_hrv_nested$circadian_per_day
