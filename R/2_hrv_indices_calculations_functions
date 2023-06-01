######################### Time domain ####################################################


# time domain function

time_domain_func <- function(hrv_id_values) {

    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR()  %>%
        # RHRV::FilterNIHR() %>%  #consider with an without (no evidence for doing this in physio net data)
        RHRV::InterpolateNIHR() %>%
        RHRV::CreateTimeAnalysis()

    rr_data$TimeAnalysis[[1]]$meanHR <- mean(rr_data$Beat$niHR)

    hrv_estimates <- data.frame(SDNN = rr_data$TimeAnalysis[[1]]$SDNN,
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


rhrv_file_time_domain_week <- function(hrv_id_values) {

    ibi_values <- cumsum(hrv_id_values$ibi/1000) # /1000: change form ms to s

    rr_data_week <- time_domain_func(ibi_values)
    rr_data_week <- as.data.frame(rr_data_week)
    date <- data.frame(real_time = hrv_id_values$real_time[1])
    rr_data_week <- cbind(date, rr_data_week)

    return(rr_data_week)
}

# Daily HRV

rhrv_file_time_domain_day <- function(hrv_id_values) {

    ibi_values <- cumsum(hrv_id_values$ibi/1000) # /1000: change form ms to s

    rr_data_day <- lapply(unique(hrv_id_values$day_number), function(daynumber){

        rr_data_day_hrv <- hrv_id_values %>%
            select(ibi, day_number) %>%
            filter(day_number == daynumber)

        ibi_val <- cumsum(rr_data_day_hrv$ibi/1000)

        rr_data_day_hrv <- time_domain_func(ibi_val)
        rr_data_day_hrv <- as.data.frame(rr_data_day_hrv)
        day_number <- data.frame(day_number = daynumber,
                                 hours_measurement = sum(hrv_id_values$ibi[hrv_id_values$day_number == daynumber])/1000/60/60,
                                 week_day = hrv_id_values$week_day[hrv_id_values$day_number == daynumber][1],
                                 real_time = hrv_id_values$real_time[hrv_id_values$day_number == daynumber][1])
        rr_data_day_hrv <- cbind(day_number, rr_data_day_hrv)
    })

    rr_data_day <- do.call(rbind,rr_data_day)

    return(rr_data_day)
}


##### Hourly HRV

rhrv_file_time_domain_hour <- function(hrv_id_values) {

    rr_data_hour <- lapply(unique(hrv_id_values$hour), function(hourly){

        rr_data_hour_hrv <- hrv_id_values %>%
            select(ibi, hour) %>%
            filter(hour == hourly)

        ibi_val <- cumsum(rr_data_hour_hrv$ibi/1000)

        rr_data_hour_hrv <- time_domain_func(ibi_val)
        rr_data_hour_hrv <- as.data.frame(rr_data_hour_hrv)
        hour <- data.frame(hour = hourly,
                                 hours_measurement = sum(hrv_id_values$ibi[hrv_id_values$hour == hourly])/1000/60/60,
                                real_time = hrv_id_values$real_time[hrv_id_values$hour == hourly][1])
        rr_data_hour_hrv <- cbind(hour, rr_data_hour_hrv)
    })

    rr_data_hour <- do.call(rbind, rr_data_hour)

    return(rr_data_hour)
}


## Circadian


rhrv_file_time_domain_circadian <- function(hrv_id_values) {

    rr_data_circ <- lapply(unique(hrv_id_values$circadian_time_points), function(circadian){

        rr_data_circ_hrv <- hrv_id_values %>%
            select(ibi, circadian_time_points) %>%
            filter(circadian_time_points == circadian)

        ibi_val <- cumsum(rr_data_circ_hrv$ibi/1000)

        rr_data_circ_hrv <- time_domain_func(ibi_val)
        rr_data_circ_hrv <- as.data.frame(rr_data_circ_hrv)
        circ <- data.frame(circadian = circadian,
                           hours_measurement = sum(hrv_id_values$ibi[hrv_id_values$circadian_time_points == circadian])/1000/60/60,
                           real_time = hrv_id_values$real_time[hrv_id_values$circadian_time_points == circadian][1]

        )
        rr_data_circ_hrv <- cbind(circ, rr_data_circ_hrv)
    })

    rr_data <- do.call(rbind, rr_data_circ)

    return(rr_data)
}


## Hour each day


rhrv_file_time_domain_hour_day <- function(hrv_id_values) {
    rr_data_hour_day <- lapply(unique(hrv_id_values$day_number), function(day){
        hrv_id_values <-  hrv_id_values %>%
            filter(day_number == day)

    rr_data_hour <- lapply(unique(hrv_id_values$hour), function(hourly){

        rr_data_hour_hrv <- hrv_id_values %>%
            select(ibi, hour) %>%
            filter(hour == hourly)

        ibi_val <- cumsum(rr_data_hour_hrv$ibi/1000)

        rr_data_hour_hrv <- time_domain_func(ibi_val)
        rr_data_hour_hrv <- as.data.frame(rr_data_hour_hrv)
        hour <- data.frame(hour = hourly,
                           hours_measurement = sum(hrv_id_values$ibi[hrv_id_values$hour == hourly])/1000/60/60,
                           day_number = hrv_id_values$day_number[1],
                           week_day = hrv_id_values$week_day[hrv_id_values$day_number == hourly][1],
                           real_time = hrv_id_values$real_time[hrv_id_values$hour == hourly][1]
        )


        rr_data_hour_hrv <- cbind(hour, rr_data_hour_hrv)
    })

    rr_data_hour <- do.call(rbind,rr_data_hour)
    })
    rr_data_hour_day <- do.call(rbind,rr_data_hour_day)

    return(rr_data_hour_day)
}


# Circadian by day

rhrv_file_time_domain_circ_day <- function(hrv_id_values) {

    rr_data_circ_day <- lapply(unique(hrv_id_values$day_number), function(day){
        hrv_id_values <-  hrv_id_values %>%
            filter(day_number == day)

        rr_data_circ <- lapply(unique(hrv_id_values$circadian_time_points), function(circadian){

            rr_data_circ_hrv <- hrv_id_values %>%
                select(ibi, circadian_time_points) %>%
                filter(circadian_time_points == circadian)

            ibi_val <- cumsum(rr_data_circ_hrv$ibi/1000)

            rr_data_circ_hrv <- time_domain_func(ibi_val)
            rr_data_circ_hrv <- as.data.frame(rr_data_circ_hrv)
            circ <- data.frame(circadian = circadian,
                               hours_measurement = sum(hrv_id_values$ibi[hrv_id_values$circadian_time_points == circadian])/1000/60/60,
                               day_number = hrv_id_values$day_number[1],
                               week_day = hrv_id_values$week_day[hrv_id_values$day_number == circadian][1],
                               real_time = hrv_id_values$real_time[hrv_id_values$circadian_time_points == circadian][1]
            )
            rr_data_circ_hrv <- cbind(circ, rr_data_circ_hrv)
        })

        rr_data <- do.call(rbind, rr_data_circ)
    })

     rr_data_circ_day <- do.call(rbind,rr_data_circ_day)

    return(rr_data_circ_day)
}




######################### Frequency domain ####################################################

# Create function for analysing hrv using individual vector values

rhrv_file_freq_domain <- function(hrv_id_values) {

    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR()  %>%
        ## RHRV::FilterNIHR() %>%  #consider with an without
        RHRV::InterpolateNIHR() %>%
        RHRV::CreateFreqAnalysis() %>%
        RHRV::CalculatePowerBand(size = 600,shift = 30)

    hrv_estimates <- data.frame(HF = rr_data$FreqAnalysis[[1]]$HF,
                                LF = rr_data$FreqAnalysis[[1]]$LF,
                                ULF = rr_data$FreqAnalysis[[1]]$ULF,
                                VLF = rr_data$FreqAnalysis[[1]]$VLF,
                                LFHF = rr_data$FreqAnalysis[[1]]$LFHF)

    return(hrv_estimates)
}

