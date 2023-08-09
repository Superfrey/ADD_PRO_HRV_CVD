############# Generate date, day, hour

create_time_day_variables <- function(data) {
  cut_points <- c(0, 6, 12, 18, 24)
  hour_labels <- c("00_06", "06_12", "12_18", "18_24")

  data <- data %>%
    dplyr::mutate(
      datetime = as.POSIXct(Real_Time, format = "%d-%m-%Y %H:%M:%S"),
      day = lubridate::day(datetime),
      week_day = lubridate::wday(datetime),
      hour = as.numeric(lubridate::hour(datetime)),
      day_number = as.integer(as.Date(datetime) - min(as.Date(datetime))),
      circadian_time_points = cut(hour, breaks = cut_points, labels = hour_labels, right = FALSE)
    )

  return(data)
}

###########
calculate_ibi <- function(data) {

    set.seed(1234)

    data <- data %>%
        dplyr::filter(
            timepoint >= 0,
            dplyr::between(Mean_HR, 25, 250),
            dplyr::between(Upper_HR, 25, 250),
            dplyr::between(Lower_HR, 25, 250)
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

    ibi_data <-   data %>%
        dplyr::full_join(ibi, by = join_by(timepoint))

    return(ibi_data)
}

##############
