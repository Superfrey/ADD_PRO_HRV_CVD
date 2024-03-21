# Resample into 30-second intervals
library(zoo)
library(forecast)
interval_length <- 30  # in seconds
ibi_30s <- rollapply(data, width = interval_length, by = interval_length, FUN = function(x) {
    60000/mean(x)
}, align = "right")

# Calculate prediction interval for each interval
prediction_intervals <- rollapply(data, width = interval_length, by = interval_length, FUN = function(x) {
    forecast(auto.arima(x), h = 1)$prediction[,1:2]
}, align = "right")

timepoints <- index(ibi_30s)
