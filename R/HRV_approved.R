## Approved algorithm

files <- list.files(here("XXXXXXXXXXXX"), pattern = "*.CSV", full.names = T)

#############
#Function for extracting ID number from file name
extract_prono <- function(paths){
    paths %>%
        fs::path_file() %>%
        stringr::str_extract("#.*#") %>%
        stringr::str_to_lower() %>%
        stringr::str_remove_all("#|pro")
}

########################

hr_hour <- function(data) {

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
           "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds","hour","day_number", week_day,"timepoint")

return(data)

}


############################ Heart beat intervals #############

ibi_function <- function(data) {
    ibi_data <- data %>%
        select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
               "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
               "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds")

    ibi_data <- ibi_data %>%
        filter( timepoint >= 0)

    ibi_data <- ibi_data %>%
        mutate(mean_ibi = 60000/Mean_HR,
               upper_ibi = 60000/Lower_HR,
               lower_ibi = 60000/Upper_HR)

    ibi <- list()

    timepoint <- ibi_data$timepoint

    for(i in timepoint) {
        data_ibi <- ibi_data %>%
            filter(timepoint == i) %>%
            select(mean_ibi,upper_ibi,lower_ibi)

        mean <- data_ibi$mean_ibi

        n <- round(30000/mean)

        sd <- (data_ibi$upper_ibi-data_ibi$lower_ibi)/(2*1.96)

        ibi_val <- rnorm(n,mean = mean,sd = sd)

        ibi <- c(ibi, list(ibi_val))

    }

    ibi <- unlist(ibi)

    return(ibi)}

#sym_low <- data_ibi$mean_ibi - data_ibi$lower_ibi #symmatry check
#sym_up <-  data_ibi$upper_ibi - data_ibi$mean_ibi
#actiheart_data <- vroom(actiheart_file)
#data_ibi <- ibi_function(actiheart_data)


######################## heart rate #####################################################

######################### Time domain ####################################################

# Create function for analysing hrv using individual vector values

rhrv_file_time_domain <- function(hrv_id_values) {

    hrv_id_values <- cumsum(hrv_id_values)

    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR()  %>%
        # RHRV::FilterNIHR() %>%  #consider with an without (no evidence for doing this in physionet data)
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

## Function for loading data and transforming into hrv indices and assign id column
# issue

vroom_hrv <- function(filepath) {
    data <- vroom(filepath)

    ibi_hour <- data  %>%
        mutate(time_hour = as.numeric(nrow(data))/2/60)

    ibi <-  ibi_function(data)

    ibi <- ibi/1000

    hrv_time_domain <- rhrv_file_time_domain(ibi)

    #hrv_freq_domain <- rhrv_file_freq_domain(ibi)

    # Merging columns together stariting with ID number

    pro_no <- extract_prono(filepath)

    hrv_data <- cbind(pro_no, hrv_time_domain)

    #hrv_data <- cbind(hrv_data, hrv_freq_domain)

    hrv_data$hour <- ibi_hour$time_hour[1]

    return(hrv_data)
}


## Function looping multiple csv files and row binding each individual into a complete data frame

hrv_loop_folder <- function(folder){

    map_dfr(folder, possibly(vroom_hrv))
}

head(hrv_data_sim2)
## Run function for the folder
hrv_data_sim1_td <- hrv_loop_folder(files[1:100]) #
usethis::use_data(hrv_data_sim1_td, overwrite = T)

hrv_data_sim2_td <- hrv_loop_folder(files[101:200]) #
usethis::use_data(hrv_data_sim2_td, overwrite = T)

hrv_data_sim3_td <- hrv_loop_folder(files[201:300])
usethis::use_data(hrv_data_sim3_td, overwrite = T)


hrv_data_sim4_td <- hrv_loop_folder(files[301:400])
usethis::use_data(hrv_data_sim4_td, overwrite = T)


hrv_data_sim5_td <- hrv_loop_folder(files[401:500])
usethis::use_data(hrv_data_sim5_td, overwrite = T)


hrv_data_sim6_td <- hrv_loop_folder(files[501:1000])
usethis::use_data(hrv_data_sim6_td, overwrite = T)


hrv_data_sim7_td <- hrv_loop_folder(files[1001:1500])
usethis::use_data(hrv_data_sim7_td, overwrite = T)


hrv_data_sim8_td <- hrv_loop_folder(files[1500:1816])
usethis::use_data(hrv_data_sim8_td, overwrite = T)

#



## Safe data

usethis::use_data(hrv_data_sim1, overwrite = T)




head(hrv_data_sim)


#Sa
usethis::use_data(hrv_data_sim, overwrite = T)

vroom_write(hrv_data_sim, here("data/hrv_data_sim.csv"))
