
hr <- 60000/(ibi*1000)

CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi2) %>%
     BuildNIHR() %>%
PlotNIHR(xlim = c(200,400))

##########################################################
ibi2 <- ibi*1000
ibi_ran2 <- ran_ibi*1000

ibi2 <- as.data.frame(ibi2)
ibi2$ibi2 <- ibi2


hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)

hrv.data$beat <- ibi2 %>%
    mutate(Time = cumsum(ibi2 / 1000) ,
           niHR = 60000/ibi2,
           RR = ibi2/1000)

hrv.data <- RHRV::BuildNIHR(hrv.data)

dat_a <- RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::BuildNIHR(hrv.data$beat)  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    CalculatePSD(indexFreqAnalysis = 1,
                 method = "lomb", doPlot = F) %>%
    PlotPSD(indexFreqAnalysis = 1)




    RHRV::CalculatePowerBand(size = 600,shift = 30)
