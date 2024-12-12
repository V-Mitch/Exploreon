create_dist_table <- function(data){
  
  distributions <- c("norm", "exp", "gamma", "lnorm", "weibull", "beta", 
                     "cauchy", "logis", "t", "unif", "pareto", "chisq", "f")
  
  fit_distributions <- function(data, distributions) {
    fits <- list()
    
    for (dist in distributions) {
      message(sprintf("Fitting %s distribution...", dist))
      
      tryCatch({
        if (dist == "beta") {
          # Check if data needs rescaling for beta distribution
          if (min(data) < 0 || max(data) > 1) {
            message("Rescaling data to [0, 1] for beta distribution.")
            data_rescaled <- (data - min(data)) / (max(data) - min(data))
            fits[[dist]] <- fitdist(data_rescaled, dist,  start = list(shape1 = 1, shape2 = 1), 
                                    control = list(maxit = 1000))
          } else {
            fits[[dist]] <- fitdist(data, dist)
          }
        } else if (dist == "t" | dist == "f" | dist == "chisq") {
          # Provide starting value for degrees of freedom (df) for t-distribution
          fits[[dist]] <- fitdist(data, dist, start = list(df = 10))
        } else {
          # Fit other distributions with original data
          fits[[dist]] <- fitdist(data, dist)
        }
      }, error = function(e) {
        message(sprintf("Failed to fit %s: %s", dist, e$message))
      })
    }
    
    return(fits)
  }
  
  # Example Usage
  fits <- fit_distributions(data, distributions)
  
  # Inspect results
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
    rownames(top_distributions) <- NULL
    colnames(top_distributions)[2] <- "p_value"
    colnames(top_distributions)[3] <- "Test.Against.Dist"
    
    return(top_distributions)
  }
  
  results_table <- get_top_distributions(gof, fits) 
  
  return(results_table)
}

generate_vertical_mini_histogram <- function(data, bins = 10, max_height = 7, symbol = "■") {
  # Create histogram breaks
  hist_res <- hist(data, breaks = bins, plot = FALSE)
  
  # Scale frequencies to fit the max_height
  scaled_freqs <- round(hist_res$counts / max(hist_res$counts) * max_height)
  
  # Generate the vertical histogram as a character vector
  lines <- lapply(max_height:1, function(level) {
    sapply(scaled_freqs, function(x) if (x >= level) symbol else " ")
  })
  
  # Combine the histogram into a single structure
  histogram <- sapply(lines, paste, collapse = " ")
  
  # Return the histogram as a structured character vector
  return(histogram)
}


combine_table_and_histogram <- function(table, histogram) {
  # Convert the table to printable character rows with proper spacing
  table_rows <- apply(table, 1, function(row) sprintf("%-10s | %-20s | %-15s", row[1], row[2], row[3]))
  
  # Ensure the histogram has the same length as the table rows
  if (length(histogram) < nrow(table)) {
    histogram <- c(histogram, rep("", nrow(table) - length(histogram)))
  }
  
  # Combine the table with its corresponding histogram row by row
  combined_output <- mapply(function(table_row, hist_row) {
    paste(table_row, "|", hist_row)
  }, c(table_rows, 
       rep(sprintf("%-10s | %-20s | %-15s", "" , "", ""),length(histogram) - length(table_rows))), histogram)
  
  # Prepare the header for the table
  header <- sprintf("%-10s | %-20s | %-15s | Histogram", "Name", "p_value", "Test.Against.Dist")
  separator <- paste(rep("-", nchar(header)), collapse = "")
  cli::cli_h1(cli::col_green("Combined Table and Histograms"))
  
  # Print the header and separator
  cat(header, "\n")
  cat(separator, "\n")
  
  # Print each combined row
  cat(combined_output, sep = "\n")
}




graphtest <- generate_vertical_mini_histogram(data)

combine_table_and_histogram(tabletest, graphtest)


graphtest <- generate_vertical_mini_histogram(data)

combine_table_and_histogram(tabletest, graphtest)


combine_table_and_histogram(tabletest, graphtest)