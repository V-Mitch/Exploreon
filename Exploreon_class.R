library(R6)
library(cli)
library(data.table)
library(fitdistrplus)
library(diptest)
source("display_paginated.R")
source("format_data_for_display.R")
source("L0_functions.R")
source("L2_functions.R")

Exploreon <- R6Class(
  "Exploreon",
  
  private = list(
    
    generate_L0_stats = function(data, round_digits = 2) {
      
      data_stats <- list(
        Formats = class(data),
        Dimensions = get_dimensions(data),
        Size = get_dynamic_memory_size(data)
      )
      return(format_data_for_display(data_stats))
    },
    
    generate_L1_stats = function(data, round_digits = 2) {
      if (!is.data.frame(data)) stop("Input must be a data frame.")
      
      # Helper function: Check numeric and round
      round_numeric <- function(x) {
        if (is.numeric(x)) round(x, round_digits) else x
      }
      # Create a list of metrics for each variable
      metrics <- list(
        Type.Internal = sapply(data, typeof),
        Type.R = sapply(data, class),
        Min = sapply(data, function(x) if (is.numeric(x)) round_numeric(min(x, na.rm = TRUE)) else NA),
        Q1 = sapply(data, function(x) if (is.numeric(x)) round_numeric(
          quantile(x, probs = 0.25, na.rm = TRUE)) else NA),
        Mean = sapply(data, function(x) if (is.numeric(x)) round_numeric(mean(x, na.rm = TRUE)) else NA),
        Median = sapply(data, function(x) if (is.numeric(x)) round_numeric(median(x, na.rm = TRUE)) else NA),
        Mode = sapply(data, function(x) {
          ux <- unique(na.omit(x))
          mode_value <- ux[which.max(tabulate(match(x, ux)))]
          round_numeric(mode_value)
        }),
        Q3 = sapply(data, function(x) if (is.numeric(x)) round_numeric(
          quantile(x, probs = 0.25, na.rm = TRUE)) else NA),
        Max = sapply(data, function(x) if (is.numeric(x)) round_numeric(max(x, na.rm = TRUE)) else NA),
        Std.Dev = sapply(data, function(x) if (is.numeric(x)) round_numeric(sd(x, na.rm = TRUE)) else NA),
        Unique.Pct = paste0(sapply(data, function(x) round_numeric(length(unique(x)) / length(x) * 100)),"%"),
        Unique.Count = paste0(sapply(data, function(x) round_numeric(length(unique(x))))),
        NA.Pct = paste0(sapply(data, function(x) round_numeric(sum(is.na(x)) / length(x) * 100)),"%"),
        NA.Count = paste0(sapply(data, function(x) round_numeric(sum(is.na(x)))))
      )
      
      return(format_data_for_display(metrics, data))
      
    },
    
    generate_L2_stats = function(data, round_digits = 2) {
      if (!is.data.frame(data)) stop("Input must be a data frame.")
      
      L2_result_per_column <- list()
      
      for (c in 1:ncol(data)){
        suppressMessages({
          suppressWarnings({
            dist_table <- create_dist_table(data[,c])
            mini_histogram <- generate_vertical_mini_histogram(data[,c])
            colname <- colnames(data)[c]
            temp_column_list <- list(dist_table, mini_histogram, colname)
            L2_result_per_column <- append(L2_result_per_column, list(temp_column_list))
          })
        })
      }
      
      return(L2_result_per_column)
    }
    
  ),
  
  public = list(
    # Instance variable to store the data
    data = NULL,
    data_name = NULL,
    # Constructor
    initialize = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame.")
      self$data <- data
      self$data_name <- deparse(substitute(data))
    },
    get_L0_stats = function(round_digits = 2) {
      # Styled header
      cli::cli_h1(cli::col_blue("{.bold Data Topview (L0) for {self$data_name}}"))
      
      # Generate and print the summary statistics without altering structure
      summary_stats <- private$generate_L0_stats(self$data, round_digits)
      print((summary_stats), row.names = FALSE)
      
      return(invisible(summary_stats))
    },
    # Method for summary statistics
    get_L1_stats = function(round_digits = 2, columns_per_page = 8) {
      # Styled header
      cli::cli_h1(cli::col_green("{.bold Basic Summary (L1) for {self$data_name}}"))
      # Generate and print the summary statistics without altering structure
      summary_stats <- private$generate_L1_stats(self$data, round_digits)
      
      # Display the summary statistics with pagination
      display_paginated(
        summary_stats, 
        columns_per_page = columns_per_page, 
        title_fn = function(page, num_pages) {
          cli::cli_h2(cli::col_blue("Page {page}/{num_pages}"))
        }
      )
      
      # print((summary_stats), row.names = FALSE)
      
      return(invisible(summary_stats))
    },
    
    get_L2_stats = function(round_digits = 2){
      
      cli::cli_h1(cli::col_silver("{.bold Distribution Summary (L2) for {self$data_name}}"))
      
      L2_result_per_column <- private$generate_L2_stats(self$data, round_digits)
      
      for (col in 1:length(L2_result_per_column)){
        invisible(combine_table_and_histogram(L2_result_per_column[[col]][[1]], 
                                              L2_result_per_column[[col]][[2]],
                                              L2_result_per_column[[col]][[3]]))
      }
    }
    
  )
)
