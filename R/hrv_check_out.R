# For preparation
library(here)
source(here("R/load_packages.R"))
file <- here("data-raw/rr-interval/006.txt")
data <- vroom(file, delim = ",")

names(data)[1] <- "ibi"

######################## heart rate #####################################################
ibi <- data$ibi/1000
ran_ibi <- sample(ibi)

######################### Time domain ####################################################

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with, when having many beats
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateTimeAnalysis()
no_shuf_HRV <- rr_data$TimeAnalysis
no_shuf_HRV


rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ran_ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an with when having many beats
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateTimeAnalysis()
shuf_HRV <- rr_data$TimeAnalysis
shuf_HRV


time_test <- cbind(shuf_HRV[[1]], no_shuf_HRV[[1]])
time_test

# Create function for analysing hrv using individual vector values


######################### Frequency domain ####################################################
ibi <- data$ibi/1000
ran_ibi <- sample(ibi)

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    #RHRV::FilterNIHR() %>%  #consider with an without FIND OUT WHAT THIS ARE DOING! _Including make diff results...
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = 600,shift = 30)

no_shuf_HRV <- rr_data$FreqAnalysis
no_shuf_HRV

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ran_ibi) %>%
    RHRV::BuildNIHR()  %>%
    #RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = 600, shift = 30)

shuf_HRV <- rr_data$FreqAnalysis
shuf_HRV

freq_test <- cbind(shuf_HRV[[1]], no_shuf_HRV[[1]])

freq_test
time_test

# Create function for analysing hrv using individual vector values
