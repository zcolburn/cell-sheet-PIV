# Perform image cross-correlation velocimetry -----------------------------

# Zach Colburn
# May 2, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383
# Windows subsystem for linux: Ubuntu 16.04.3 LTS
# ImageJ 1.52a, Java 1.8.0_66 (64-bit)
# PIV plugin: https://sites.google.com/site/qingzongtseng/piv

# This script will allow you to do the following:
# 1) Collect data from the user.
# 2) Save the file fiji_runner.sh to the user selected output folder.
# 
# After running this script, the file fiji_runner.sh should be run with the 
# Windows subsystem for linux. This can be accomplished by navigating to the 
# output folder (where fiji_runner.sh is located), clicking the address bar and
# typing bash then enter. Next, type "./fiji_runner.sh" (without quotes). The 
# data will begin to process. Make sure that you have:
# 1) Exported folders of image sequences to the data directory from a .lif file 
# using these parameters: Export to TIFF --> lossless data compression, use 
# directories, save RAW data.
# 2) Created an output directory within the output folder before running this 
# script.
# 
# IMPORTANT: Do not forget to set the path to the ImageJ/Fiji executable below. 
# The path should be the path as referenced from the Windows subsystem for 
# linux.

fijiPath <- "/mnt/c/Users/Jonathan/fiji-win64/Fiji.app/ImageJ-win64.exe"



# Acquire user data -------------------------------------------------------

svDialogs::dlg_message(
  "Your data folders can be nested to an arbitrary depth. Folder names for 
  mixed cultures should be prefixed as follows: 
  'NullCellName_NullCellProportion_to_PlusCellName_PlusCellProportion-...'. 
  Folder names for sheets composed of pure cultures should be  prefixed with 
  'CellName-...'. Do not include periods in folder names. The ... should be 
  replaced with any valid text. Names for the same cell type need to match 
  between experiments."
)

# Choose the data directory.
svDialogs::dlg_message("Select the data directory.")
rawdataDir <- svDialogs::dlgDir("RawData")$res

# Choose the output directory.
svDialogs::dlg_message("Select the output directory.")
dataDir <- svDialogs::dlgDir("Data")$res

# Next, set analysis parameters.
svDialogs::dlg_message("Set analysis parameters.")

# Identify the phase channel number.
invalid <- TRUE
while(invalid){
  ch_Phase <- svDialogs::dlg_input(
    message = "Phase channel number", default = "1"
  )$res
  invalid <- !grepl("^[0-9]$", ch_Phase)
}

# Identify the GFP channel number.
invalid <- TRUE
while(invalid){
  ch_GFP <- svDialogs::dlg_input(
    message = "GFP channel number", default = "2"
  )$res
  invalid <- !grepl("^[0-9]$", ch_GFP)
}

# Identify the imaging interval.
invalid <- TRUE
while(invalid){
  interval <- svDialogs::dlg_input(
    message = "Interval (s)", default = "300"
  )$res
  invalid <- !grepl("^[0-9]+$", interval) || (interval <= 0)
}

# Identify the number of microns in the x direction (for pixel to micron 
# conversions).
invalid <- TRUE
while(invalid){
  microns_in_x <- svDialogs::dlg_input(
    message = "X width (microns)", default = "1281.71"
  )$res
  invalid <- !grepl("^[0-9]*[.]?[0-9]*$", microns_in_x) || (microns_in_x <= 0)
}

# Specify the number of pixels in the x axis (for generating PIV images).
invalid <- TRUE
while(invalid){
  width <- svDialogs::dlg_input(message = "Pixels in x", default = 1392)$res
  invalid <- !grepl("^[0-9]+$", width) || (width <= 0)
}

# Specify the number of pixels in the y axis (for generating PIV images).
invalid <- TRUE
while(invalid){
  height <- svDialogs::dlg_input(message = "Pixels in y", default = 1040)$res
  invalid <- !grepl("^[0-9]+$", height) || (height <= 0)
}

# Specify the names of the null cell types.
invalid <- TRUE
while(invalid){
  null_cell <- svDialogs::dlg_input(
    message = "Null cell name(s) (comma separated list)", default = "JEB"
  )$res
  if(grepl("^[_a-zA-Z0-9,]+$", null_cell)){
    split_name <- unlist(strsplit(null_cell, ","))
    for(i in 1:length(split_name)){
      if(!grepl("^[_a-zA-Z]+[_a-zA-Z0-9]*$", split_name[i])){
        invalid <- TRUE
        break
      }else{
        invalid <- FALSE
      }
    }
  }
}

# Specify the names of the positive cell types.
invalid <- TRUE
while(invalid){
  plus_cell <- svDialogs::dlg_input(
    message = "Positive cell name(s) (comma separated list)", default = "JEB"
  )$res
  if(grepl("^[_a-zA-Z0-9,]+$", plus_cell)){
    split_name <- unlist(strsplit(plus_cell, ","))
    for(i in 1:length(split_name)){
      if(!grepl("^[_a-zA-Z]+[_a-zA-Z0-9]*$", split_name[i])){
        invalid <- TRUE
        break
      }else{
        invalid <- FALSE
      }
    }
  }
}

# Identify the GFP protein's identity. The order of the list needs to 
# correspond to the order of the positive cell types given above.
invalid <- TRUE
while(invalid){
  gfp_protein <- svDialogs::dlg_input(
    message = "GFP protein (no spaces allowed)", default = "beta4_integrin"
  )$res
  if(grepl("^[_a-zA-Z0-9,]+$", gfp_protein)){
    split_name <- unlist(strsplit(gfp_protein, ","))
    for(i in 1:length(split_name)){
      if(!grepl("^[_a-zA-Z]+[_a-zA-Z0-9]*$", split_name[i])){
        invalid <- TRUE
        break
      }else{
        invalid <- FALSE
      }
    }
  }
}

# Declare any relevant notes about the data
notes <- svDialogs::dlg_input(
  message = "Notes", 
  default = paste0("Time run: ", Sys.time())
)$res

# Save analysis parameters.
parameters <- list(
  "Raw data [rawdataDir]: " = rawdataDir,
  "Data [dataDir]: " = dataDir,
  "Phase channel [ch_Phase]: " = ch_Phase,
  "GFP channel [ch_GFP]: " = ch_GFP,
  "Null cell name [null_cell]: " = null_cell,
  "Positive cell name [plus_cell]: " = plus_cell,
  "GFP protein [gfp_protein]: " = gfp_protein,
  "Interval (s) [interval]: " = interval,
  "Microns in x (microns) [microns_in_x]: " = microns_in_x,
  "Pixels in x [width]: "= width,
  "Pixels in y [height]: " = height,
  "Notes [notes]: " = notes
)
parametersOut <- data.frame(
  "Col1" = names(parameters),
  "Col2" = unlist(parameters, use.names = FALSE)
)
file_Experiment <- file.path(dataDir, "Experiment.txt")
write.table(
  tibble::as_tibble(parametersOut),
  file.path(file_Experiment),
  row.names = FALSE, col.names = FALSE,
  quote = FALSE
)


# Create processing guide files and fiji_runner.sh ------------------------

# Find files and split the tasks for parallel processing in Fiji.
dirs <- list.files(rawdataDir, recursive = TRUE, full.names = TRUE)
dirs <- dirs[!grepl("MetaData", dirs)]
outputDirDepth <- length(unlist(strsplit(rawdataDir, "/")))
dirs <- strsplit(dirs, "/")
dirs <- dirs[!unlist(lapply(dirs, function(i){length(i) <= outputDirDepth+1}))]
dirs <- lapply(dirs, function(i){paste(i[1:(length(i)-1)], collapse = "/")})
dirs <- unlist(dirs)
dirs <- unique(dirs)

# Recreate the raw data folder structure in the data directory.
endings <- gsub(paste0(rawdataDir,"/"), "", dirs)
new_folders <- file.path(dataDir, endings)
lapply(new_folders, function(i){dir.create(i, recursive = TRUE)})

# Get the nubmer of cores to use.
max_num_cores <- parallel::detectCores()
default_num_cores <- max_num_cores
if(max_num_cores > 1){
  default_num_cores <- max_num_cores - 1
}
invalid <- TRUE
while(invalid){
  num_cores <- svDialogs::dlg_input(
    message = "Number of cores to use", default = default_num_cores
  )$res
  invalid <- !((!grepl("^[0-9]+$", num_cores) == FALSE) && 
    ((!(num_cores <= 0) & !(num_cores > max_num_cores)) == TRUE))
}
num_cores <- as.integer(num_cores)

# Write processing guide files (pgf)
if(length(dirs) < num_cores){
  num_cores <- length(dirs)
}
splitDirs <- split(dirs, 1:num_cores)

for(i in 1:num_cores){
  file_pgf <- file.path(dataDir, paste0("pgf_", i, ".txt"))
  write.table(
    tibble::as_tibble(splitDirs[[i]]),
    file.path(file_pgf),
    row.names = FALSE, col.names = FALSE,
    quote = FALSE
  )
}

# Create fiji_runner.sh data.
# 
# This file should have a header line, one line for executing operations on 
# the contents of each of the processing guide files, and then a new line.
# Each thread will open a separate instance of ImageJ and process data in a 
# background thread. Data written to the log is exported to the log file 
# corresponding to the processing guide file number.
fiji_runner_lines <- c("#!/bin/bash")
script <- "MR"
for(i in 1:num_cores){
  macroFile <- file.path(getwd(), "Scripts", paste0(script, ".ijm"))
  
  refFile <- gsub(
    "/",
    "\\\\\\\\",
    file.path(dataDir, paste0("pgf_", i, ".txt"))
  )
  
  logFile <- file.path(dataDir, paste0("log_", i, ".txt"))
  logFile <- gsub(":", "", logFile)
  logFile <- unlist(strsplit(logFile,"/"))
  logFile[1] <- tolower(logFile[1])
  logFile <- paste(logFile, collapse = "/")
  logFile <- paste0("/mnt/", logFile)
  
  syscmd <- paste0(
    "'",fijiPath,"' -batch '",macroFile,"' '",refFile,"' > '",logFile,"' &"
  )
  
  fiji_runner_lines <- c(fiji_runner_lines, syscmd)
}
fiji_runner_lines <- c(fiji_runner_lines, "")

# Write fiji_runner.sh to the output folder.
file_fiji_runner <- file.path(dataDir, "fiji_runner.sh")
con <- file(file.path(file_fiji_runner), "wb")
write.table(
  tibble::as_tibble(fiji_runner_lines),
  con,
  row.names = FALSE, col.names = FALSE,
  quote = FALSE, sep = "",
  eol = "\n"
)
close(con)

# Instruct the user to run fiji_runner.sh
svDialogs::dlgMessage(
  "Open Ubuntu bash in the output directory you selected, then run 
  ./fiji_runner.sh"
)
