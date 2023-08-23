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
      year_day = yday(datetime),
      day_number = year_day-min(year_day),
      circadian_time_points = cut(hour, breaks = cut_points, labels = hour_labels, right = FALSE)
    )

  return(data)
}

###########
calculate_ibi <- function(data) {
  set.seed(1234)

  data <- data %>%
    dplyr::filter(
      timepoint >= 120,
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

  ibi_data <- data %>%
    dplyr::full_join(ibi, by = join_by(timepoint))

  return(ibi_data)
}


############## Physical activity data

numextract <- function(string){
    str_extract(string, "\\-*\\d+\\.*\\d*")}

pa_data_preperation <- function(pa_data){

 pa_data <- pa_data %>%
     dplyr::group_by(id) %>%
     dplyr::mutate(pro_no = numextract(id)) %>%
     dplyr::mutate(pro_no = as.numeric(pro_no))

 pa_data <- pa_data %>%
     dplyr::mutate(
         datetime = as.POSIXct(TIMESTAMP, format = "%d-%m-%Y %H:%M:%S"),
         day = lubridate::day(datetime),
         week_day = lubridate::wday(datetime),
         hour = as.numeric(lubridate::hour(datetime)),
         year_day = yday(datetime))

 pa_data <- pa_data %>%
     group_by(pro_no) %>%
     mutate(day_number = year_day-min(year_day),
            day_num = dayofyear - min(dayofyear))


  pa_data <- pa_data %>%
     select(pro_no,datetime, hour, day_number,day_num,year_day,
            hourofday, dayofweek, dayofyear,
            INT_stdMET_highIC_BRANCH_cat6,INT_2MET_Branch7, SLEEP, sr_sleep) %>%
      rename(time_frach_MET_under_25 = INT_stdMET_highIC_BRANCH_cat6,
             time_spend_MET_above_3 = INT_2MET_Branch7)

    return(pa_data)
}

#pa_data <- pa_data_preperation(hourly_means_df)
