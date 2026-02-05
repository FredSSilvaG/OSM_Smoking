# calculate the prevalence at each threshold
count_prevalence_steps <- function(column) {
  # Define the sequence of prevalence thresholds
  # Adapt the value until where, such that we can see small counts if necessary
  thresholds <- seq(1, 0, by = -0.05)
  
  # Count the occurrences for each threshold
  counts <- sapply(thresholds, function(threshold) sum(column >= threshold))
  
  # Return the counts as a named vector
  names(counts) <- thresholds
  return(counts)
}

# plots the calculated prevalences for each threshold
plot_prevalence_counts <- function(prevalence_counts) {
  # Plot the results
  plot(
    x = names(prevalence_counts), # Names of the vector as x-axis labels
    y = prevalence_counts,        # Counts as y values
    type = "o",                   # "o" for lines and points
    xlab = "Prevalence Threshold", 
    ylab = "Counts", 
    main = "Number of species with a certain prevalence",
    col = "darkblue",
    pch = 16 # Filled circles for points
  )
  
  # Rotate x-axis labels for better readability
  axis(1, at = seq_along(names(prevalence_counts)), labels = names(prevalence_counts), las = 2, cex.axis=0.8)
  
  # Add grid lines for better readability
  grid()
}

