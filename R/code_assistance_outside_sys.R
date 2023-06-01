error_messages <- c()
stopped_iteration <- 0
completed_iterations <- 0

for (i in 1:2000) {
  tryCatch({
    # Code that might throw an error
    result <- summary(data[i, ])
    
    # Increment the completed_iterations count
    completed_iterations <- completed_iterations + 1
    
    # Continue with the rest of the code
    # ...
  }, error = function(e) {
    # Append the error message to the vector
    error_messages <- c(error_messages, paste("Error occurred at iteration", i, ":", e$message))
    
    # Store the stopped iteration count
    stopped_iteration <<- i
    
    # Skip to the next iteration
    next
  })
  
  # Periodically save error data to a file
  if (i %% 100 == 0) {
    tryCatch({
      error_df <- data.frame(
        Iteration = 1:length(error_messages),
        Error_Message = error_messages,
        Stopped_Iteration = stopped_iteration,
        Completed_Iterations = completed_iterations
      )
      
      write.csv(error_df, file = "error_data.csv", row.names = FALSE)
    }, error = function(e) {
      cat("Error occurred while saving error data:", e$message, "\n")
    })
  }
}

# Save the final error data
final_error_df <- data.frame(
  Iteration = 1:length(error_messages),
  Error_Message = error_messages,
  Stopped_Iteration = stopped_iteration,
  Completed_Iterations = completed_iterations
)
write.csv(final_error_df, file = "error_data.csv", row.names = FALSE)
