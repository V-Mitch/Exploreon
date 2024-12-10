library(R6)
library(cli)
library(data.table)

format_data_for_display <- function(metrics_data, original_data = NULL) {
  # Convert the list to a long-format data.frame  
  summary_stats <- as.data.frame(metrics_data)
  if(is.null(original_data)){
    rownames(summary_stats) <- colnames(metrics_data) 
  }else{
    rownames(summary_stats) <- colnames(original_data)  # Set variable names as rownames
  }
  # Transpose the data frame
  transposed_summary_stats <- as.data.frame(t(summary_stats))
  # Make the variable names a column instead of rownames
  # absence of original data, then no need for header
  if(is.null(original_data)){
    transposed_summary_stats <- cbind(. = rownames(transposed_summary_stats), transposed_summary_stats)
    colnames(transposed_summary_stats) <- transposed_summary_stats[1,]
    transposed_summary_stats <- transposed_summary_stats[-1,]
  # header case
  }else{
    transposed_summary_stats <- cbind(. = rownames(transposed_summary_stats), transposed_summary_stats)
  }
  rownames(transposed_summary_stats) <- NULL
  return(transposed_summary_stats)
}

Exploreon <- R6Class(
  "Exploreon",
  
  private = list(
    
    generate_data_stats = function(data, round_digits = 2) {
      
      get_dynamic_memory_size <- function(data) {
        size_in_bytes <- as.numeric(object.size(data))
        
        if (size_in_bytes < 1024) {
          return(paste(size_in_bytes, "Bytes"))
        } else if (size_in_bytes < 1024^2) {
          size_in_kb <- size_in_bytes / 1024
          return(paste(round(size_in_kb, 2), "KB"))
        } else if (size_in_bytes < 1024^3) {
          size_in_mb <- size_in_bytes / (1024^2)
          return(paste(round(size_in_mb, 2), "MB"))
        } else {
          size_in_gb <- size_in_bytes / (1024^3)
          return(paste(round(size_in_gb, 2), "GB"))
        }
      }  
      get_dimensions <- function(data) {
        dims <- dim(data)
        # Handle cases where dim() is NULL (e.g., for vectors)
        if (is.null(dims)) {
          return(paste0("Length: ", length(data)))
        }
        # Dynamically build dimension labels
        labels <- paste(dims, collapse = ", ")
        return(labels)
      }
      data_stats <- list(
        Formats = class(data),
        Dimensions = get_dimensions(data),
        Size = get_dynamic_memory_size(data)
      )
      return(format_data_for_display(data_stats))
    },
    
    generate_summary_stats = function(data, round_digits = 2) {
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
        Max = sapply(data, function(x) if (is.numeric(x)) round_numeric(max(x, na.rm = TRUE)) else NA),
        Mean = sapply(data, function(x) if (is.numeric(x)) round_numeric(mean(x, na.rm = TRUE)) else NA),
        Median = sapply(data, function(x) if (is.numeric(x)) round_numeric(median(x, na.rm = TRUE)) else NA),
        Mode = sapply(data, function(x) {
          ux <- unique(na.omit(x))
          mode_value <- ux[which.max(tabulate(match(x, ux)))]
          round_numeric(mode_value)
        }),
        Std.Dev = sapply(data, function(x) if (is.numeric(x)) round_numeric(sd(x, na.rm = TRUE)) else NA),
        Unique.Pct = paste0(sapply(data, function(x) round_numeric(length(unique(x)) / length(x) * 100)),"%"),
        NA.Pct = paste0(sapply(data, function(x) round_numeric(sum(is.na(x)) / length(x) * 100)),"%")
      )
      
      return(format_data_for_display(metrics, data))
      
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
    get_data_stats = function(round_digits = 2) {
      # Styled header
      cli::cli_h1(cli::col_blue("{.bold Data Topview (L0) for {self$data_name}}"))
      
      # Generate and print the summary statistics without altering structure
      summary_stats <- private$generate_data_stats(self$data, round_digits)
      print((summary_stats), row.names = FALSE)
      
      return(invisible(summary_stats))
    },
    # Method for summary statistics
    get_summary_stats = function(round_digits = 2) {
      # Styled header
      cli::cli_h1(cli::col_green("{.bold Basic Summary (L1) for {self$data_name}}"))
      
      # Generate and print the summary statistics without altering structure
      summary_stats <- private$generate_summary_stats(self$data, round_digits)
      print((summary_stats), row.names = FALSE)
      
      return(invisible(summary_stats))
    }
    
  )
)
