# For preparation
library(here)
source(here("R/load_packages.R"))
file <- here("data-raw/rr-interval/006.txt")
data <- vroom(file, delim = ",")

names(data)[1] <- "ibi"


###################################################
#Function for extracting ID number from file name
#extract_prono <- function(paths){
 #   paths %>%
  #      fs::path_file() %>%
   #     stringr::str_extract("#.*#") %>%
    #    stringr::str_to_lower() %>%
     #   stringr::str_remove_all("#|pro")
#}

########################
#names(actiheart_data)
#ibi_function <- function(data) {
#    ibi_data <- data %>%
#        select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
#               "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
#               "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds")#

#    ibi_data <- ibi_data %>%
#        filter(timepoint >= 0)

#    ibi_data <- ibi_data %>%
#        mutate(mean_ibi = 60000/Mean_HR,
 #              upper_ibi = 60000/Lower_HR,
 #              lower_ibi = 60000/Upper_HR)

  #  return(ibi_data)}

#sym_low <- data_ibi$mean_ibi - data_ibi$lower_ibi #symmatry check
#sym_up <-  data_ibi$upper_ibi - data_ibi$mean_ibi
#actiheart_data <- vroom(actiheart_file)
#data_ibi <- ibi_function(actiheart_data)


############################ Heart beat intervals #############

#ibi_function <- function(data) {
    #ibi_data <- data %>%
    #    select("Mean_HR", "Upper_HR", "Lower_HR","timepoint", "Real_Time",
     #          "max_ibi_1_in_milliseconds","max_ibi_2_in_milliseconds",
     #          "min_ibi_1_in_milliseconds", "min_ibi_2_in_milliseconds")

   # ibi_data <- ibi_data %>%
   #     filter( timepoint >= 0)

    #ibi_data <- ibi_data %>%
    #    mutate(mean_ibi = 60000/Mean_HR,
     #          upper_ibi = 60000/Lower_HR,
      #         lower_ibi = 60000/Upper_HR)

    #ibi <- list()

    #timepoint <- ibi_data$timepoint

   # for(i in timepoint) {
      #  data_ibi <- ibi_data %>%
       #     filter(timepoint == i) %>%
        #    select(mean_ibi,upper_ibi,lower_ibi)

        #mean <- data_ibi$mean_ibi

        #n <- round(30000/mean)

        #sd <- (data_ibi$upper_ibi-data_ibi$lower_ibi)/(2*1.96)

        #ibi_val <- rnorm(n,mean = mean,sd = sd)

       # ibi <- c(ibi, list(ibi_val))

 #   }

  #  ibi <- unlist(ibi)

 #   return(ibi)}

#ini <- ibi_function(actiheart_data)


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

    hrv_estimates <- data.frame(SDNN = rr_data$TimeAnalysis[[1]]$SDNN,
                                SDANN = rr_data$TimeAnalysis[[1]]$SDANN,
                                SDNNIDX = rr_data$TimeAnalysis[[1]]$SDNNIDX,
                                TINN = rr_data$TimeAnalysis[[1]]$TINN,
                                HRVi = rr_data$TimeAnalysis[[1]]$HRVi,
                                RMSSD = rr_data$TimeAnalysis[[1]]$rMSSD,
                                pNN50 = rr_data$TimeAnalysis[[1]]$pNN50,
                                SDSD = rr_data$TimeAnalysis[[1]]$SDSD,
                                )

    return(hrv_estimates)
}

######################### Frequency domain ####################################################
ibi <- data$ibi/1000
sum_sec <- sum(ibi)
ibi <- cumsum(ibi)

ran_ibi <- data %>%
    mutate(group = cumsum(ibi) %/% 30000)
ran_ibi <- ran_ibi %>%
    group_by(group) %>%
    mutate(ibi= sample(ibi))
ran_ibi <- (cumsum(ran_ibi$ibi))/1000

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = sum_sec, shift = sum_sec, indexFreqAnalysis = 1, type = "fourier")


no_shuf_HRV <- rr_data$FreqAnalysis
no_shuf_HRV

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ran_ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = sum_sec, shift = sum_sec, indexFreqAnalysis = 1,  type = "fourier")

shuf_HRV <- rr_data$FreqAnalysis
shuf_HRV

filter <- cbind(shuf_HRV[[1]],no_shuf_HRV[[1]])
filter


###################################################################


ibi <- cumsum(data$ibi/1000)
#######################
ran_ibi <- data %>%
    mutate(group = cumsum(ibi) %/% 30000)

# Randomize values within each group
ran_ibi <- ran_ibi %>%
    group_by(group) %>%
    mutate(ibi= sample(ibi))
ran_ibi <- (cumsum(ran_ibi$ibi))/1000

rr_data <- RHRV::CreateHRVData()
rr_data <- RHRV::LoadBeatVector(rr_data,ibi)
rr_data <- RHRV::BuildNIHR(rr_data)
rr_data <- RHRV::FilterNIHR(rr_data)
rr_data <- RHRV::InterpolateNIHR(rr_data) #freqhr = 4
rr_data <- RHRV::CreateTimeAnalysis(rr_data)
rr_data <- RHRV::CreateFreqAnalysis(rr_data)
#rr_data <- RHRV::CalculatePowerBand(rr_data, size = 600, shift = 30)

#rr_data$FreqAnalysis[[1]]
 #                                   size = 8000, shift = 8000,
  #                                  sizesp = 8000,
   #                                 type = "fourier",
    #                                ULFmin = 0, ULFmax = 0.03,
     #                               VLFmin = 0.03, VLFmax = 0.05,
      #                              LFmin = 0.05, LFmax = 0.15,
       #                             HFmin = 0.15, HFmax = 0.4)

mean_frq_domain <- as.data.frame(rr_data$FreqAnalysis[[1]]) %>%
    mutate(HFmean = mean(HF),
           LFmean = mean(LF),
           LF_HFmean = mean(LFHF))

mean_frq_domain <- mean_frq_domain %>%
    select(HFmean, LFmean, LF_HFmean) %>%
    slice(1)

mean_frq_domain
#########################################################################


plot_frq <- plot_frq %>%
    mutate(TPmean = mean(HRV),
           HFmean = mean(HF),
           LFmean = mean(LF),
           LF_HFmean = mean(LFHF))
non_order <- plot_frq %>%
    select(TPmean,HFmean,LFmean,LF_HFmean) %>%
    slice(1)

order <- plot_frq %>%
select(TPmean,HFmean,LFmean,LF_HFmean) %>%
    slice(1)

cbind(order,non_order)



names(rr_data$FreqAnalysis[[1]]$HRV)
summary(rr_data$FreqAnalysis[[1]]$HRV)
plot_frq <- as.data.frame(rr_data$FreqAnalysis[[1]])

mean_frq_domain <- as.data.frame(rr_data$FreqAnalysis[[1]]) %>%
    mutate(HFmean = mean(HF),
           LFmean = mean(LF),
           LF_HFmean = mean(LFHF))

mean_frq_domain <- mean_frq_domain %>%
    select(HFmean, LFmean, LF_HFmean) %>%
    slice(1)

head(plot_frq)


rr_data <-CalculatePowerBand(rr_data,
                   type = "wavelet",
                   bandtolerance = 0.01,
                   relative = FALSE)

spectrogram <- PlotSpectrogram(HRVData = rr_data,
                               size = 600, shift = 60,
                               scale = "logaritmic",
                               freqRange = c(-0.4, 0.4))

PlotPowerBand(rr_data, ymax = 3000, ymaxratio = 1.7)



    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = 600,shift = 30)

no_shuf_HRV <- rr_data$FreqAnalysis
no_shuf_HRV







# Create function for analysing hrv using individual vector values

rhrv_file_freq_domain <- function(hrv_id_values) {
    sum_sec <- sum(hrv_id_values)

    rr_data <-
        RHRV::CreateHRVData() %>%
        RHRV::LoadBeatVector(hrv_id_values) %>%
        RHRV::BuildNIHR()  %>%
        ## RHRV::FilterNIHR() %>%  #consider with an without
        RHRV::InterpolateNIHR() %>%
        RHRV::CreateFreqAnalysis() %>%
        RHRV::CalculatePowerBand(size = sum_sec, shift = sum_sec)

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
