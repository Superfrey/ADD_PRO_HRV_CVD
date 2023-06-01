# For preparation
library(here)
source(here("R/load_packages.R"))
file <- here("data-raw/rr-interval/006.txt")
data <- vroom(file, delim = ",")

names(data)[1] <- "ibi"

######################## heart rate #####################################################
ibi <- data$ibi/1000
ibi <- cumsum(ibi)
sum_sec <- sum(ibi)

ran_ibi <- ran_ibi %>%
    group_by(group) %>%
    mutate(ibi= sample(ibi))
ran_ibi <- (cumsum(ran_ibi$ibi))/1000


######################### Time domain ####################################################

######################### Time domain ####################################################

ibi <- cumsum(ibi)


rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateTimeAnalysis()
no_shuf_HRV <- rr_data$TimeAnalysis[[1]]


#########################
rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ran_ibi) %>%
    RHRV::BuildNIHR()  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateTimeAnalysis()
shuf_HRV <- rr_data$TimeAnalysis[[1]]


######################### Frequency domain ####################################################
ibi <- data$ibi/1000
ran_ibi <- sample(ibi)

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    #RHRV::FilterNIHR() %>%  #consider with an without FIND OUT WHAT THIS ARE DOING! _Including make diff results...
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis()%>%
    RHRV::CalculatePowerBand(size = 600,shift = 30)

spectogram_freq <- PlotSpectrogram(rr_data, size = 600, shift = 30,
                                   scale = "logaritmic",
                                   freqRange = c(0,0.4))
plot(spectogram_freq)

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
    #RHRV::CreateFreqAnalysis() %>%
    #RHRV::CalculatePowerBand(size = sum_sec, shift = sum_sec, type = "fourier") %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePSD(indexFreqAnalysis = 1,
                                method = "lomb", doPlot = FALSE) %>%
    RHRV::CalculateEnergyInPSDBands(indexFreqAnalysis = 1)


no_shuf_HRV <- rr_data
no_shuf_HRV

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ran_ibi) %>%
    RHRV::BuildNIHR()  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    #RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePSD(indexFreqAnalysis = 1,
                       method = "lomb", doPlot = F)

shuf_HRV <- rr_data
shuf_HRV

filter <- cbind(shuf_HRV[[1]],no_shuf_HRV[[1]])
filter


###################################################################
