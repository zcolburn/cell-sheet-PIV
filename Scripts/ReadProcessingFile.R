# ReadProcessingFile ------------------------------------------------------


# Zach Colburn
# May 2, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This function will take the path to Processing.txt, read the file, and return 
# a list of experimental parameters.

ReadProcessingFile <- function(file_processing){
  processing_file <- readLines(file_processing)
  processing_file <- strsplit(processing_file, "\\[|\\]:  ")
  
  parameters <- sapply(processing_file, "[[", 3)
  names(parameters) <- sapply(processing_file, "[[", 2)
  parameters <- as.list(parameters)
  
  parameters$max_central_pegs <- as.integer(parameters$max_central_pegs)
  parameters$max_neighbors <- as.integer(parameters$max_neighbors)
  parameters$max_t_forward <- as.numeric(parameters$max_t_forward)
  parameters$max_distance_microns <- as.integer(parameters$max_distance_microns)
  parameters$dist_interval_microns <- as.integer(parameters$dist_interval_microns)
  parameters$num_cores <- as.numeric(parameters$num_cores)
  
  return(parameters)
}
