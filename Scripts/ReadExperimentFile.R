# ReadExperimentFile ------------------------------------------------------


# Zach Colburn
# May 2, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This function will take the path to Experiment.txt, read the file, and return 
# a list of experimental parameters.

ReadExperimentFile <- function(file_experiment){
  experiment_file <- readLines(file_experiment)
  experiment_file <- strsplit(experiment_file, "\\[|\\]:  ")
  
  parameters <- sapply(experiment_file, "[[", 3)
  names(parameters) <- sapply(experiment_file, "[[", 2)
  parameters <- as.list(parameters)
  
  parameters$ch_Phase <- as.integer(parameters$ch_Phase)
  parameters$ch_GFP <- as.integer(parameters$ch_GFP)
  parameters$microns_in_x <- as.numeric(parameters$microns_in_x)
  parameters$width <- as.integer(parameters$width)
  parameters$height <- as.integer(parameters$height)
  parameters$interval <- as.numeric(parameters$interval)
  
  return(parameters)
}
