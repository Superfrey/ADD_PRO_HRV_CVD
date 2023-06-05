############# Generate date, day, hour

actiheart_time_day <- function(data) {
  cut_points <- c(0, 6, 12, 18, 24)
  hour_labels <- c("00_06", "06_12", "12_18", "18_24")

  data <- data %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(Real_Time),
      day = lubridate::day(datetime),
      week_day = lubridate::wday(datetime),
      hour = as.numeric(lubridate::hour(datetime)),
      day_number = as.integer(as.Date(datetime) - min(as.Date(datetime))),
      circadian_time_points = cut(hour, breaks = cut_points, labels = hour_labels, right = FALSE)
    )

  return(data)
}

###########
ibi_function <- function(data) {
  data <- data %>%
    dplyr::filter(
      timepoint >= 0,
      dplyr::between(Mean_HR, 250, 25),
      dplyr::between(Upper_HR, 250, 25),
      dplyr::between(Lower_HR, 250, 25)
    ) %>%
    dplyr::mutate(
      mean_ibi = 60000 / Mean_HR,
      upper_ibi = 60000 / Lower_HR,
      lower_ibi = 60000 / Upper_HR
    )
  ibi <- data %>%
    dplyr::group_split(timepoint) %>%
    purrr::map_dfr(~ {
      mean <- .x$mean_ibi
      n <- round(30000 / mean)
      sd <- (.x$upper_ibi - .x$lower_ibi) / (2 * 1.96)

      tibble::tibble(
        timepoint = .x$timepoint,
        ibi = stats::rnorm(n, mean = mean, sd = sd)
      )
    })

  ibi_data <- data %>%
    dplyr::full_join(ibi)

  return(ibi_data)
}


# sym_low <- data_ibi$mean_ibi - data_ibi$lower_ibi #symmatry check
# sym_up <-  data_ibi$upper_ibi - data_ibi$mean_ibi


## IBI diff data
ibi_diff_data <- function(ibi_data) {
  ibi_diff <- ibi_data %>%
    filter(
      timepoint >= 0,
      is.na(max_ibi_2_in_milliseconds) == FALSE,
      is.na(min_ibi_2_in_milliseconds) == FALSE,
      min_ibi_2_in_milliseconds > 300,
      min_ibi_2_in_milliseconds < 1500,
      max_ibi_2_in_milliseconds < 1500,
      max_ibi_2_in_milliseconds > 300
    ) %>%
    mutate(ibi_diff = max_ibi_2_in_milliseconds - min_ibi_2_in_milliseconds)

  return(ibi_diff)
}
