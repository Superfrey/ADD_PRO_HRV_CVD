
hr <- 60000/(ibi*1000)

CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi2) %>%
     BuildNIHR() %>%
PlotNIHR(xlim = c(200,400))

##########################################################
ibi2 <- cumsum(ibi)
ibi_ran2 <- ran_ibi*1000

ibi2 <- as.data.frame(ibi2)
ibi2$ibi2 <- ibi2


rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi2) %>%
    RHRV::BuildNIHR()  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    CalculatePSD(indexFreqAnalysis = 1,
                 method = "lomb", doPlot = F) %>%
    PlotPSD(indexFreqAnalysis = 1)




rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi2) %>%
    RHRV::BuildNIHR()  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateTimeAnalysis()
no_shuf_HRV <- rr_data$TimeAnalysis
no_shuf_HRV


rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi2) %>%
    RHRV::BuildNIHR()  %>%
    #RHRV::FilterNIHR() %>%  #consider with an without FIND OUT WHAT THIS ARE DOING! _Including make diff results...
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = 600,shift = 30)