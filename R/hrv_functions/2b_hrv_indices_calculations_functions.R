######################### Time domain ####################################################


# time domain function

calculation_time_domain_hrv <- function(hrv_id_values) {
    hrv_id_values <- cumsum(hrv_id_values / 1000)

    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR() %>%
        # RHRV::FilterNIHR() %>%  #consider with an without (no evidence for doing this with physionet data, and in the generated IBI we have no abnormal values)
        RHRV::InterpolateNIHR() %>%
        RHRV::CreateTimeAnalysis()

    rr_data$TimeAnalysis[[1]]$meanHR <- mean(rr_data$Beat$niHR)

    hrv_estimates <- data.frame(
        SDNN = rr_data$TimeAnalysis[[1]]$SDNN,
        SDANN = rr_data$TimeAnalysis[[1]]$SDANN,
        SDNNIDX = rr_data$TimeAnalysis[[1]]$SDNNIDX,
        TINN = rr_data$TimeAnalysis[[1]]$TINN,
        HRVi = rr_data$TimeAnalysis[[1]]$HRVi,
        RMSSD = rr_data$TimeAnalysis[[1]]$rMSSD,
        pNN50 = rr_data$TimeAnalysis[[1]]$pNN50,
        SDSD = rr_data$TimeAnalysis[[1]]$SDSD,
        meanHR = rr_data$TimeAnalysis[[1]]$meanHR
    )

    return(hrv_estimates)
}


# week HRV

rhrv_time_domain_week <- function(hrv_id_values) {
    if (nrow(hrv_id_values) < 180) {

        variables <- c("SDNN","SDANN","SDNNIDX","TINN","HRVi","RMSSD","pNN50","SDSD","meanHR","start_time","end_time","time_hour")
        df_na <- data.frame(matrix(NA, nrow= 1, ncol = length(variables)))
        colnames(df_na) <- variables

        return(df_na)
    } else {
    rr_data_week <- tibble::tibble(calculation_time_domain_hrv(hrv_id_values$ibi))

    rr_data_week <- rr_data_week %>%
        dplyr::mutate(
            start_time = hrv_id_values$datetime[1],
            end_time = tail(hrv_id_values[["datetime"]], n = 1),
            time_hour = as.numeric(length(unique(hrv_id_values$timepoint))) / 2 / 60
        )


    return(rr_data_week)
    }
    }


# Daily HRV

rhrv_time_domain_day <- function(hrv_id_values) {
    if (nrow(hrv_id_values) < 180) {

        variables <- c("SDNN","SDANN","SDNNIDX","TINN","HRVi","RMSSD","pNN50","SDSD","meanHR","start_time","time_hour","week_day", "day_number")
        df_na <- data.frame(matrix(NA, nrow= 1, ncol = length(variables)))
        colnames(df_na) <- variables

        return(df_na)
    } else {

    rr_data_day <- hrv_id_values %>%
        dplyr::group_split(day_number) %>%
        purrr::map_df(possibly(~{
            day_hrv_df <- tibble::tibble(calculation_time_domain_hrv(.x$ibi))
            day_hrv_df <- day_hrv_df %>%
                dplyr::mutate(
                    day_number = .x$day_number[1],
                    week_day = .x$week_day[1],
                    time_hour = as.numeric(length(unique(.x$timepoint))) / 2 / 60,
                    start_date = .x$datetime[1]
                )
        }))

    return(rr_data_day)
}}


## Hour each day

rhrv_time_domain_hour_day <- function(hrv_id_values) {
    if (nrow(hrv_id_values) < 180) {

        variables <- c("SDNN","SDANN","SDNNIDX","TINN","HRVi","RMSSD","pNN50","SDSD","meanHR","hour","week_day","time_hour","start_date",
                       "day_number")
        df_na <- data.frame(matrix(NA, nrow= 1, ncol = length(variables)))
        colnames(df_na) <- variables

        return(df_na)
    } else {
    rr_data_hour_day <- hrv_id_values %>%
        dplyr::group_split(day_number, hour) %>%
        purrr::map_df(possibly(~ {
            hour_day_hrv_df <- tibble::tibble(calculation_time_domain_hrv(.x$ibi))

            hour_day_hrv_df <- hour_day_hrv_df %>%
                dplyr::mutate(
                    hour = .x$hour[1],
                    day_number = .x$day_number[1],
                    week_day = .x$week_day[1],
                    time_hour = as.numeric(length(unique(.x$timepoint))) / 2 / 60,
                    start_date = .x$datetime[1]
                )
        }))

    return(rr_data_hour_day)
    }
}

# Circadian by day

rhrv_time_domain_circ_day <- function(hrv_id_values) {
    if (nrow(hrv_id_values) < 180) {

        variables <- c("SDNN","SDANN","SDNNIDX","TINN","HRVi","RMSSD","pNN50","SDSD","meanHR","start_time","end_time",
                       "time_hour","week_day", "day_number", "circadian_time_points")
        df_na <- data.frame(matrix(NA, nrow= 1, ncol = length(variables)))
        colnames(df_na) <- variables

        return(df_na)
    } else {
    rr_data_circ_day <- hrv_id_values %>%
        dplyr::group_split(day_number, circadian_time_points) %>%
        purrr::map_df(possibly(~ {
            circ_day__hrv_df <- tibble::tibble(calculation_time_domain_hrv(.x$ibi))

            circ_day_hrv_df <- circ_day__hrv_df %>%
                dplyr::mutate(
                    circadian_time_points = .x$circadian_time_points[1],
                    day_number = .x$day_number[1],
                    week_day = .x$week_day[1],
                    time_hour = as.numeric(length(unique(.x$timepoint))) / 2 / 60,
                    start_date = .x$datetime[1]
                )
        }))

    return(rr_data_circ_day)
}

}
######################### Frequency domain ####################################################

# Create function for analyzing hrv using individual vector values

rhrv_file_freq_domain <- function(hrv_id_values) {
    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR() %>%
        ## RHRV::FilterNIHR() %>%  #consider with an without
        RHRV::InterpolateNIHR() %>%
        RHRV::CreateFreqAnalysis() %>%
        RHRV::CalculatePowerBand(size = 600, shift = 30)

    hrv_estimates <- data.frame(
        HF = rr_data$FreqAnalysis[[1]]$HF,
        LF = rr_data$FreqAnalysis[[1]]$LF,
        ULF = rr_data$FreqAnalysis[[1]]$ULF,
        VLF = rr_data$FreqAnalysis[[1]]$VLF,
        LFHF = rr_data$FreqAnalysis[[1]]$LFHF
    )

    return(hrv_estimates)
}


###########################PHYSICAL ACTIVITY BASED FUNCTION#######################################


###################################function######################

# Daily HRV

rhrv_time_domain_day_pa <- function(hrv_id_values) {

    if (nrow(hrv_id_values) < 180) {

        variables <- c("SDNN","SDANN","SDNNIDX","TINN","HRVi","RMSSD","pNN50","SDSD","meanHR","start_time",
                       "day_number", "weekday, time_hour")
        df_na <- data.frame(matrix(NA, nrow= 1, ncol = length(variables)))
        colnames(df_na) <- variables

        return(df_na)
    } else {

    rr_data_day <- hrv_id_values %>%
        dplyr::group_by(day_number, add. = TRUE) %>%
        dplyr::group_split() %>%
        purrr::map_df(possibly(~{
            day_hrv_df <- tibble::tibble(calculation_time_domain_hrv(.x$ibi))
            day_hrv_df <- day_hrv_df %>%
                dplyr::mutate(
                    day_number = .x$day_number[1],
                    week_day = .x$week_day[1],
                    time_hour = as.numeric(length(unique(.x$timepoint))) / 2 / 60,
                )
        }))

    return(rr_data_day)
    }
    }

## Hour each day

rhrv_time_domain_hour_day_pa <- function(hrv_id_values) {
    if (nrow(hrv_id_values) < 180) {

        variables <- c("SDNN","SDANN","SDNNIDX","TINN","HRVi","RMSSD","pNN50","SDSD","meanHR","start_time",
                       "time_hour","week_day", "day_number","hour")
        df_na <- data.frame(matrix(NA, nrow= 1, ncol = length(variables)))
        colnames(df_na) <- variables

        return(df_na)
    } else {
    rr_data_hour_day <- hrv_id_values %>%
        dplyr::group_by(day_number, hour, add. = TRUE) %>%
        dplyr::group_split() %>%
        purrr::map_df(possibly(~ {
            hour_day_hrv_df <- tibble::tibble(calculation_time_domain_hrv(.x$ibi))

            hour_day_hrv_df <- hour_day_hrv_df %>%
                dplyr::mutate(
                    hour = .x$hour[1],
                    day_number = .x$day_number[1],
                    week_day = .x$week_day[1],
                    time_hour = as.numeric(length(unique(.x$timepoint))) / 2 / 60,
                    start_date = .x$datetime[1]
                )
        }))

    return(rr_data_hour_day)
    }
    }

