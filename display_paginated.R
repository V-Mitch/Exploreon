display_paginated <- function(data, columns_per_page = 6, title_fn = NULL) {
  # Determine the number of pages
  num_columns <- ncol(data)
  num_pages <- ceiling(num_columns / columns_per_page)
  
  # Loop through each page and display the content
  for (page in 1:num_pages) {
    # Subset the columns for the current page
    start_col <- (page - 1) * columns_per_page + 1
    end_col <- min(page * columns_per_page, num_columns)
    page_data <- data[, start_col:end_col, drop = FALSE]
    
    # Add row names as a separate column for reference
    if (page > 1) {
      page_data <- cbind(data[,1], page_data)
      colnames(page_data)[1] <- "."
    }
    
    # Page header
    if (!is.null(title_fn)) {
      title_fn(page, num_pages)
    } else {
      cli::cli_h2(cli::col_blue("Page {page}/{num_pages}"))
    }
    
    # Print the current page of data
    print(page_data, row.names = FALSE)
    
  }
}