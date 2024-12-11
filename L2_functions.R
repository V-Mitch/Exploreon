
# detect_distribution <- function(data){
#   # Fit multiple distributions
#   fits <- list(
#     normal = fitdist(data, "norm"),
#     exponential = fitdist(data, "exp"),
#     gamma = fitdist(data, "gamma")
#   )
#   
#   # Goodness-of-fit comparison
#   gof <- gofstat(fits)
#   # Goodness-of-fit statistics
#   # Invert statistics (lower is better)
#   inverse_stats <- 1 / stats
#   
#   # Calculate mean inverse fit score for each distribution
#   mean_scores <- colMeans(inverse_stats)
#   
#   # Normalize to probabilities
#   combined_weights <- mean_scores / sum(mean_scores)
#   
#   # Convert to percentages
#   combined_weights_percent <- round(combined_weights * 100, 2)
#   names(combined_weights_percent) <- c("Normal", "Exponential", "Gamma")
#   print(combined_weights_percent)
# }
gof_for_dists <- function(data){
  # List of continuous distributions to test
  distributions <- c("norm", "exp", "gamma", "lnorm", "weibull", "beta", 
                     "cauchy", "logis", "t", "unif", "pareto", "chisq", "f")
  
  # Fit all distributions
  fits <- list()
  for (dist in distributions) {
    try({
      fits[[dist]] <- fitdist(data, dist, start = list(df = 5))
    }, silent = TRUE)
  }
  return(fits)
}

gof <- gofstat(fits)


get_top_distributions <- function(gof_results, fits, metric = "chisqpvalue", significance_levels = c(0.10, 0.05, 0.01)) {
  # Check if the chosen metric exists in the gof_results
  if (!metric %in% names(gof_results)) {
    stop(paste("Metric", metric, "is not found in the goodness-of-fit results."))
  }
  
  # Extract the selected metric
  fit_stats <- as.data.frame(gof_results[[metric]])
  fit_stats$Internal_Name <- rownames(fit_stats)  # Add internal names as a column
  
  # Map back to the original names using the order of `fits`
  original_names <- names(fits)
  fit_stats$Name <- sapply(seq_along(original_names), function(i) {
    if (i <= nrow(fit_stats)) {
      return(original_names[i])
    } else {
      return(NA)
    }
  })
  
  # Rename columns for clarity
  colnames(fit_stats) <- c("Metric_Value", "Metric_Value", "Name")
  
  # Sort by the chosen metric (lower is better)
  fit_stats <- fit_stats[order(fit_stats$Metric_Value, decreasing = TRUE), ]
  
  # Select the top 3 distributions
  top_distributions <- head(fit_stats, 3)
  
  # Add asterisks for significance levels
  top_distributions$Significance <- sapply(top_distributions$Metric_Value, function(value) {
    if (value <= significance_levels[3]) {
      return("***")  # 1% significance
    } else if (value <= significance_levels[2]) {
      return("**")   # 5% significance
    } else if (value <= significance_levels[1]) {
      return("*")    # 10% significance
    } else {
      return("")     # No asterisk
    }
  })
  
  # Reorganize columns for better readability
  top_distributions <- top_distributions[, c("Name", "Metric_Value", "Significance")]
  
  return(top_distributions)
}
