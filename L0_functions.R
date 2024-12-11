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