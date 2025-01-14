create_dist_table <- function(data){
  
  distributions <- c("norm", "exp", "gamma", "lnorm", "weibull", "beta", 
                     "cauchy", "logis", "t", "unif", "pareto", "chisq", "f")
  
  fit_distributions <- function(data, distributions) {
    fits <- list()
    
    # Check for degenerate distribution
    if (length(unique(data)) == 1) {
      message("Data is degenerate (all values are the same). Returning degenerate fits.")
      for (dist in distributions) {
        fits[[dist]] <- list(
          estimate = unique(data), 
          loglik = -Inf, 
          warning = "Degenerate distribution, cannot fit."
        )
      }
      return(fits)
    }
    
    # Check for near-degenerate data (e.g., most values are the same)
    most_frequent_value <- names(sort(table(data), decreasing = TRUE))[1]
    proportion_most_frequent <- max(table(data)) / length(data)
    if (proportion_most_frequent > 0.95) { # Threshold: 95% of data has the same value
      message(sprintf("Data is near-degenerate (%.2f%% of values are '%s'). No distributions will be fitted.", 
                      proportion_most_frequent * 100, most_frequent_value))
      
      for (dist in distributions) {
        fits[[dist]] <- list(
          estimate = unique(data), 
          loglik = -Inf
        )
        return(NULL)
      }
    }
    browser()
    
    
    for (dist in distributions) {
      message(sprintf("Fitting %s distribution...", dist))
      capture.output(
        
        tryCatch({
          if (dist == "beta") {
            # Check if data needs rescaling for beta distribution
            if (min(data) < 0 || max(data) > 1) {
              message("Rescaling data to [0, 1] for beta distribution.")
              data_rescaled <- (data - min(data)) / (max(data) - min(data))
              fits[[dist]] <- fitdist(data_rescaled, dist, 
                                      start = list(shape1 = 1, shape2 = 1), 
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
      )
    }
    
    return(fits)
  }
  
  # 
  fits <- fit_distributions(data, distributions)
  if (!is.null(fits)) {
    # Initialize an empty list to collect results
    gof_results <- list()
    
    for (name in names(fits)) {
      tryCatch({
        # Calculate gofstats for each fit
        message(sprintf("Calculating goodness-of-fit for %s...", name))
        gof_results[[name]] <- gofstat(fits[[name]])
      }, error = function(e) {
        message(sprintf("Failed to calculate goodness-of-fit for %s: %s", name, e$message))
        # Assign a placeholder (e.g., NULL or NA) for failed fits
        gof_results[[name]] <- list(
          chisq = NA,
          ad = NA,
          bic = NA,
          message = "Goodness-of-fit failed"
        )
      })
    }
    # Combine results into a consistent structure (if needed)
    gof <- do.call(rbind, lapply(names(gof_results), function(name) {
      cbind(distribution = name, gof_results[[name]])
    }))
  } else {
    gof <- NULL
  }
  
  
  get_top_distributions <- function(gof_results, fits, metric = "chisqpvalue", significance_levels = c(0.10, 0.05, 0.01)) {
    # Check if the chosen metric exists in the gof_results
    # if (!metric %in% names(gof_results)) {
    #   stop(paste("Metric", metric, "is not found in the goodness-of-fit results."))
    # }
    # 
    
    dist_names <- names(gof_results)
    fit_stats <- list()
    for (name in names(gof_results)){
      # Extract the selected metric
      fit_stats[name] <- gof_results[[name]][[metric]]
      # fit_stats$Internal_Name <- n  # Add internal names as a column
    }
    
    # Map back to the original names using the order of `fits`
    original_names <- names(fit_stats)
    # fit_stats$Name <- sapply(seq_along(original_names), function(i) {
    #   if (i <= nrow(fit_stats)) {
    #     return(original_names[i])
    #   } else {
    #     return(NA)
    #   }
    # })
    fit_stats <-  stack(fit_stats)
    # Rename columns for clarity
    colnames(fit_stats) <- c("P_value", "Distribution")
    
    # Sort by the chosen metric (lower is better)
    fit_stats <- fit_stats[order(fit_stats$P_value, decreasing = TRUE), ]
    
    # Select the top 3 distributions
    top_distributions <- head(fit_stats, 3)
    
    # Add asterisks for significance levels
    top_distributions$Significance <- sapply(top_distributions$P_value, function(value) {
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
    top_distributions <- top_distributions[, c("Distribution", "P_value", "Significance")]
    rownames(top_distributions) <- NULL
    colnames(top_distributions)[2] <- "P_value"
    colnames(top_distributions)[3] <- "Test.Against.Dist"
    return(top_distributions)
  }
  
  results_table <- get_top_distributions(gof_results, fits) 
  
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


combine_table_and_histogram <- function(table, histogram, colname) {
  # Convert the table to printable character rows with proper spacing
  table_rows <- apply(table, 1, function(row) sprintf("%-13s | %-15s | %-15s", row[1], row[2], row[3]))
  
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
  header <- sprintf("%-13s | %-15s | %-15s | Histogram", "Distribution", "p_value", "Test.Against.Dist")
  separator <- paste(rep("-", nchar(header)), collapse = "")
  # cli::cli_h1(cli::blue("{.Column: {self$data_name}}"))
  
  # Print the header and separator
  cat(colname, "\n")
  cat(header, "\n")
  cat(separator, "\n")
  
  # Print each combined row
  cat(combined_output, sep = "\n")
}

