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
