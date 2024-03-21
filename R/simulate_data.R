# Define parameters
num_timepoints <- 24 * 60 * 60 / 30  # 24 hours in 30-second intervals
mean_hr_mean <- 75  # Mean of the mean heart rate
mean_hr_std <- 5    # Standard deviation of the mean heart rate
pi_width <- 10      # Width of the prediction intervals (95% CI)

# Set a seed for reproducibility
set.seed(42)

# Generate random data
timepoints <- seq(0, num_timepoints - 1) * 30  # Time in seconds
mean_hr <- rnorm(num_timepoints, mean_hr_mean, mean_hr_std)
lower_pi <- mean_hr - (pi_width / 2)
upper_pi <- mean_hr + (pi_width / 2)

# Create a data frame
data <- data.frame(
    Timepoint = timepoints,
    Mean_HR = mean_hr,
    Lower_HR = lower_pi,
    Upper_HR = upper_pi
)

# Optionally, you can convert timepoints to hours or minutes for better readability
data$Timepoint <- data$Timepoint / 3600  # Convert to hours

# Add date and time information with the specified format
start_datetime <- as.POSIXct("2023-09-07 00:00:00", tz = "UTC", format = "%d-%m-%Y %H:%M:%S")
data$Real_Time <- format(start_datetime + timepoints, format = "%d-%m-%Y %H:%M:%S")

# Print the first few rows of the simulated dataset
head(data)
