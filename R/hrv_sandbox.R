
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
vec_shuf_HRV <- rr_data$TimeAnalysis
vec_shuf_HRV

rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    # RHRV::FilterNIHR() %>%  #consider with an without
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateTimeAnalysis()

r_HRV <- rr_data$TimeAnalysis
r_HRV

cbind(vec_shuf_HRV,r_HRV)

## Frequency domain


#plot HR data
rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    PlotNIHR(xlim = c(200,400))

# plot frequency

rr_frq_plot <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an without FIND OUT WHAT THIS ARE DOING! _Including make diff results...
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    CalculatePSD(indexFreqAnalysis = 1,
                 method = "lomb", doPlot = F)
PlotPSD(rr_frq_plot, indexFreqAnalysis = 1)


rr_frq_plot_ar <-
    RHRV::CreateHRVData() %>%
    RHRV::SetVerbose(FALSE) %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an without FIND OUT WHAT THIS ARE DOING! _Including make diff results...
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    CalculateSpectrogram(size = 600,
                         shift = 30)

spectrogram <- PlotSpectrogram(HRVData = rr_frq_plot_ar,
                               size = 600, shift = 60,
                               scale = "logaritmic",
                               freqRange = c(0, 0.4))


rr_data <-
    RHRV::CreateHRVData() %>%
    RHRV::LoadBeatVector(ibi) %>%
    RHRV::BuildNIHR()  %>%
    RHRV::FilterNIHR() %>%  #consider with an without FIND OUT WHAT THIS ARE DOING! _Including make diff results...
    RHRV::InterpolateNIHR() %>%
    RHRV::CreateFreqAnalysis() %>%
    RHRV::CalculatePowerBand(size = 600,shift = 30)


r_HRV <- rr_data$FreqAnalysis[[1]]
r_HRV
