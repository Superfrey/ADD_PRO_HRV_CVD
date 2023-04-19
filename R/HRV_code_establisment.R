# R code shift

##files <- ___

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
names(actiheart_data)
ibi_function <- function(data) {
    ibi_data <- data %>%
        select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
               "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
               "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds")

    ibi_data <- ibi_data %>%
        filter(timepoint >= 0)

    ibi_data <- ibi_data %>%
        mutate(mean_ibi = 60000/Mean_HR,
               upper_ibi = 60000/Lower_HR,
               lower_ibi = 60000/Upper_HR)

    return(ibi_data)}

#sym_low <- data_ibi$mean_ibi - data_ibi$lower_ibi #symmatry check
#sym_up <-  data_ibi$upper_ibi - data_ibi$mean_ibi
#actiheart_data <- vroom(actiheart_file)
#data_ibi <- ibi_function(actiheart_data)


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

ini <- ibi_function(actiheart_data)



######################## heart rate #####################################################

######################### Time domain ####################################################

# Create function for analysing hrv using individual vector values

rhrv_file_time_domain <- function(hrv_id_values) {

    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR()  %>%
        # RHRV::FilterNIHR() %>%  #consider with an without
        RHRV::InterpolateNIHR() %>%
        RHRV::CreateTimeAnalysis()

    hrv_estimates <- data.frame(SDNN = rr_data$TimeAnalysis[[1]]$SDNN,
                                RMSSD = rr_data$TimeAnalysis[[1]]$rMSSD,
                                pNN50 = rr_data$TimeAnalysis[[1]]$pNN50,
                                SDSD = rr_data$TimeAnalysis[[1]]$SDSD,
                                SDANN = rr_data$TimeAnalysis[[1]]$SDANN)

    return(hrv_estimates)
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

# Handle date and time in the dataset
data <- vroom(here("data-raw/actiheart/processed/10016_385_1273_ADDITION-PRO_#10016#_GPRProcessedFLData.csv"))



## Function for loading data and transforming into hrv indices and assign id column
# issue

vroom_hrv <- function(filepath) {
    data <- vroom(filepath)

    ibi_hour <- data  %>%
        mutate(time_hour = as.numeric(nrow(data))/2/60)

    ibi <-  ibi_function(data)

    ibi <- ibi/1000

    hrv_time_domain <- rhrv_file_time_domain(ibi)

    hrv_freq_domain <- rhrv_file_freq_domain(ibi)

    # Merging columns together stariting with ID number

    pro_no <- extract_prono(filepath)

    hrv_data <- cbind(pro_no, hrv_time_domain)

    hrv_data <- cbind(hrv_data, hrv_freq_domain)

    # hrv_data <- cbind(hrv_data, meanHR)

    # hrv_data <- cbind(hrv_data, medianHR)

    hrv_data$hour <- ibi_hour$time_hour[1]

    return(hrv_data)
}


values <- vroom_hrv(files[2])

values
## Function looping multiple csv files and row binding each individual into a complete data frame

hrv_loop_folder <- function(folder){

    map_dfr(folder, possibly(vroom_hrv))
}


#Sa
usethis::use_data(hrv_data_sim, overwrite = T)

vroom_write(hrv_data_sim, here("data/hrv_data_sim.csv"))
