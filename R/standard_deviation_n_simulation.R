# Define the given values
max_val <- 1000
second_highest_val <- 800
min_val <- 200
second_lowest_val <- 400
mean_val <- 600

# Calculate the standard deviation
sd_val <- (max_val - min_val) / (4 * qnorm(0.75) - 4 * qnorm(0.25))

# Generate a sequence of random numbers from a normal distribution
n_samples <- 1000
simulated_data <- rnorm(n_samples, mean = mean_val, sd = sd_val)

hist(simulated_data)



############################## Other Solution

# Set the values
mean <- 110
lower_pi <- 90
upper_pi <- 130
confidence_level <- 0.95


SD <- sqrt((upper_pi-mean)*55)
SD
# Calculate the critical value of Z-score
z_score <- qnorm((1 + confidence_level)/2)

# Calculate the standard deviation
sd <- (upper_pi - lower_pi) / (2 * z_score)

# Print the result
cat("Standard deviation is:", sd)

mean <- 110
n <- 1000 # number of observations

# Simulate data
simulated_data <- rnorm(n, mean, sd)

# Print the simulated data
print(simulated_data)

# Simulate data
simulated_data <- numeric(n)
simulated_data[1] <- rnorm(1, mean, sd) # generate first value
i <- 2 # start from second value

while(i <= n){
    value <- rnorm(1, mean, sd) # generate new value
    if(abs(value - simulated_data[i-1]) < 4){ # check condition
        simulated_data[i] <- value
        i <- i + 1
    }
}

simulated_data
hist(simulated_data)

################

# Set the values
mean <- 50
sd <- 5
sum_value <- 1000

# Generate data with mean and sd
obs <- numeric() # create an empty vector to store the observations
total_sum <- 0
while (total_sum < sum_value) {
    obs <- c(obs, rnorm(1, mean, sd)) # generate one observation at a time
    total_sum <- sum(obs) # calculate the current sum of observations
}
obs <- obs[-length(obs)] # remove the last observation that causes the sum to exceed the target value

# Scale the data to have sum equal to sum_value
simulated_data <- obs * (sum_value / sum(obs))

# Print the simulated data and their sum
print(simulated_data)
cat("Sum of values: ", sum(simulated_data))

