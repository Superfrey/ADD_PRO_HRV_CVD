############# Generate date, day, hour

actiheart_time_day <- function(data) {

    data$datetime <- as.POSIXct(data$Real_Time, format = "%d-%m-%Y %H:%M:%S")

    data <- data %>%
        mutate(day = lubridate::day(data$datetime),
               week_day=lubridate::wday(data$datetime),
               hour = as.numeric(lubridate::hour(data$datetime)),
               day_number = as.integer(as.Date(data$datetime) - min(as.Date(data$datetime))))

    cut_points <- c(0,6,12,18,24)
    hour_labels <- c("00_06", "06_12", "12_18", "18_24")

    data <- data %>% #remove next time
        mutate(circadian_time_points = cut(hour, breaks = cut_points, labels = hour_labels, right = FALSE))

    data <- data %>%
        select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
               "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
               "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds",
               "hour","day_number", "week_day","timepoint","circadian_time_points")

    return(data)

}

###########
ibi_function <- function(data) {

    data <- actiheart_time_day(data)


    ibi_data <- data %>%
        select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
               "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
               "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds",
               "hour","day_number", "week_day","timepoint","circadian_time_points")

    ibi_data <- ibi_data %>%
        filter(timepoint >= 0,
               Mean_HR < 250,
               Upper_HR < 250,
               Lower_HR < 250,
               Mean_HR > 25,
               Upper_HR > 25,
               Lower_HR > 25
               )

    ibi_data <- ibi_data %>%
        mutate(mean_ibi = 60000/Mean_HR,
               upper_ibi = 60000/Lower_HR,
               lower_ibi = 60000/Upper_HR)

    ibi <- lapply(unique(ibi_data$timepoint), function(i) {

        data_ibi <- ibi_data %>%
            filter(timepoint == i)  %>%
            select("mean_ibi","upper_ibi","lower_ibi","Real_Time",
                   "hour","day_number", "week_day","timepoint",
                   "circadian_time_points")

        mean <- data_ibi$mean_ibi
        n <- round(30000/mean)
        sd <- (data_ibi$upper_ibi-data_ibi$lower_ibi)/(2*1.96)

        ibi_val <- rnorm(n, mean = mean, sd = sd)

        tibble(ibi = ibi_val, day_number = data_ibi$day_number[1:length(n)] , hour = data_ibi$hour[1:length(n)],
               week_day = data_ibi$week_day[1:length(n)], circadian_time_points = data_ibi$circadian_time_points[1:length(n)],
               real_time = data_ibi$Real_Time[1:length(n)], timepoint = i)
    })

    ibi <- do.call(rbind,ibi)

    return(ibi)}


#sym_low <- data_ibi$mean_ibi - data_ibi$lower_ibi #symmatry check
#sym_up <-  data_ibi$upper_ibi - data_ibi$mean_ibi


## IBI diff data
ibi_diff_data <-  function(ibi_data) {

data <- actiheart_time_day(ibi_data)


ibi_data <- data %>%
    select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
           "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
           "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds",
           "hour","day_number", "week_day","timepoint","circadian_time_points")

ibi_data <- ibi_data %>%
    filter(timepoint >= 0, is.na(max_ibi_2_in_milliseconds)== FALSE, is.na(min_ibi_2_in_milliseconds)== FALSE,
           min_ibi_2_in_milliseconds > 300, min_ibi_2_in_milliseconds < 1500,
           max_ibi_2_in_milliseconds < 1500, max_ibi_2_in_milliseconds > 300 )


ibi_diff_data <- ibi_data %>%
    mutate(ibi_diff = max_ibi_2_in_milliseconds - min_ibi_2_in_milliseconds)

return(ibi_diff_data)

}
