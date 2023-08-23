
########################## IBI

### IBI diff for 60 sec epoch

ibi_diff_est <-  function(ibi_diff_values) {

    ibi_diff <- ibi_diff_values %>%
        mutate(mean_ibi_diff = mean(ibi_diff),
               median_ibi_diff = mean(ibi_diff),
               mean_ratio_ibi_diff = mean(min_ibi_2_in_milliseconds) / mean(max_ibi_2_in_milliseconds),
               mean_ibi_2nd_low = mean(min_ibi_2_in_milliseconds),
               mean_ibi_2nd_high = mean(max_ibi_2_in_milliseconds))


    ibi_diff_val <- ibi_diff %>%
        select(mean_ibi_diff, median_ibi_diff, mean_ratio_ibi_diff, mean_ibi_2nd_low, mean_ibi_2nd_high) %>%
        mutate(mean_ibi_diff = ifelse(mean_ibi_diff < 1| mean_ibi_diff > 1500, NA, mean_ibi_diff),
                median_ibi_diff = ifelse(mean_ibi_diff < 1| mean_ibi_diff > 1500, NA, median_ibi_diff),
                mean_ratio_ibi_diff = ifelse(mean_ibi_diff < 1| mean_ibi_diff > 1500, NA, mean_ratio_ibi_diff),
                mean_ibi_2nd_low = ifelse(mean_ibi_2nd_low < 100| mean_ibi_2nd_low > 2000, NA, mean_ibi_2nd_low),
                mean_ibi_2nd_high = ifelse(mean_ibi_2nd_high < 100| mean_ibi_2nd_high > 2000, NA, mean_ibi_2nd_high)) %>%
        slice(1)

    ibi_diff_val <- as.data.frame(ibi_diff_val)

    return(ibi_diff_val)
}


# week ibi_diff

acti_ibi_diff_week <- function(ibi_id_val) {

    rr_data_week <- ibi_diff_est(ibi_id_val)

    return(rr_data_week)
}

# Daily ibi_diff

acti_ibi_diff_day <- function(ibi_id_val) {

    rr_data_day <- lapply(unique(ibi_id_val$day_number), function(daynumber){

        rr_data_day_ibi_diff <- ibi_id_val %>%
            select(ibi_diff,  min_ibi_2_in_milliseconds,  max_ibi_2_in_milliseconds,  day_number) %>%
            filter(day_number == daynumber)

        rr_data_day_ibi_diff <- ibi_diff_est(rr_data_day_ibi_diff)
        rr_data_day_ibi_diff <- as.data.frame(rr_data_day_ibi_diff)
        day_number <- data.frame(day_number = daynumber,
                                 week_day = ibi_id_val$week_day[ibi_id_val$day_number == daynumber][1])
        rr_data_day_ibi_diff <- cbind(day_number, rr_data_day_ibi_diff)
    })

    rr_data_day <- do.call(rbind,rr_data_day)

    return(rr_data_day)
}


##### Hourly ibi_diff

acti_ibi_diff_hour <- function(ibi_id_val) {

    rr_data_hour <- lapply(unique(ibi_id_val$hour), function(hourly){

        rr_data_hour_ibi_diff <- ibi_id_val %>%
            select(ibi_diff,  min_ibi_2_in_milliseconds,  max_ibi_2_in_milliseconds,  hour) %>%
            filter(hour == hourly)

        rr_data_hour_ibi_diff <- ibi_diff_est(rr_data_hour_ibi_diff)
        rr_data_hour_ibi_diff <- as.data.frame(rr_data_hour_ibi_diff)
        hour <- data.frame(hour = hourly)
        rr_data_hour_ibi_diff <- cbind(hour, rr_data_hour_ibi_diff)
    })

    rr_data_hour <- do.call(rbind, rr_data_hour)

    return(rr_data_hour)
}



## Circadian


acti_ibi_diff_circadian <- function(ibi_id_val) {

    rr_data_circ <- lapply(unique(ibi_id_val$circadian_time_points), function(circadian){

        rr_data_circ_ibi_diff <- ibi_id_val %>%
            select(ibi_diff,  min_ibi_2_in_milliseconds,  max_ibi_2_in_milliseconds,  circadian_time_points) %>%
            filter(circadian_time_points == circadian)


        rr_data_circ_ibi_diff <- ibi_diff_est(rr_data_circ_ibi_diff)
        rr_data_circ_ibi_diff <- as.data.frame(rr_data_circ_ibi_diff)
        circ <- data.frame(circadian = circadian
        )
        rr_data_circ_ibi_diff <- cbind(circ, rr_data_circ_ibi_diff)
    })

    rr_data <- do.call(rbind, rr_data_circ)

    return(rr_data)
}


## Hour each day


acti_ibi_diff_hour_day <- function(ibi_id_val) {
    rr_data_hour_day <- lapply(unique(ibi_id_val$day_number), function(day){
        ibi_id_val <-  ibi_id_val %>%
            filter(day_number == day)

        rr_data_hour <- lapply(unique(ibi_id_val$hour), function(hourly){

            rr_data_hour_ibi_diff <- ibi_id_val %>%
                select(ibi_diff,  min_ibi_2_in_milliseconds,  max_ibi_2_in_milliseconds,  hour) %>%
                filter(hour == hourly)


            rr_data_hour_ibi_diff <- ibi_diff_est(rr_data_hour_ibi_diff)
            rr_data_hour_ibi_diff <- as.data.frame(rr_data_hour_ibi_diff)
            hour <- data.frame(hour = hourly,
                               day_number = ibi_id_val$day_number[1])


            rr_data_hour_ibi_diff <- cbind(hour, rr_data_hour_ibi_diff)
        })

        rr_data_hour <- do.call(rbind,rr_data_hour)
    })
    rr_data_hour_day <- do.call(rbind,rr_data_hour_day)

    return(rr_data_hour_day)
}


# Circadian by day

acti_ibi_diff_circ_day <- function(ibi_id_val) {

    rr_data_circ_day <- lapply(unique(ibi_id_val$day_number), function(day){
        ibi_id_val <-  ibi_id_val %>%
            filter(day_number == day)

        rr_data_circ <- lapply(unique(ibi_id_val$circadian_time_points), function(circadian){

            rr_data_circ_ibi_diff <- ibi_id_val %>%
                select(ibi_diff,  min_ibi_2_in_milliseconds,  max_ibi_2_in_milliseconds,  circadian_time_points) %>%
                filter(circadian_time_points == circadian)


            rr_data_circ_ibi_diff <- ibi_diff_est(rr_data_circ_ibi_diff)
            rr_data_circ_ibi_diff <- as.data.frame(rr_data_circ_ibi_diff)
            circ <- data.frame(circadian = circadian,
                               day_number = ibi_id_val$day_number[1]
            )
            rr_data_circ_ibi_diff <- cbind(circ, rr_data_circ_ibi_diff)
        })

        rr_data <- do.call(rbind, rr_data_circ)
    })

    rr_data_circ_day <- do.call(rbind,rr_data_circ_day)

    return(rr_data_circ_day)
}

