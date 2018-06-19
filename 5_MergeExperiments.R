# Merge summarized data ---------------------------------------------------


# Zach Colburn
# May 7, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This script will take the experiment level merged summary files, awot.rds and 
# awt.rds, and create an analysis level merged summary file, containing the 
# results from all experiments. These files are placed in the Cache directory 
# and are labeled awot.rds and awt.rds.


# Acquire user data -------------------------------------------------------

# Choose the analysis directory.
svDialogs::dlg_message("Select the analysis directory.")
analysisDir <- svDialogs::dlgDir("Output")$res
setwd(analysisDir)


# Create the Cache folder -------------------------------------------------

if(dir.exists("Cache")){unlink("Cache", recursive = TRUE, force = TRUE)}
dir.create("Cache")
cacheDir <- file.path("Cache")


# Merge experimental data -------------------------------------------------

# Find folders to merge.
files <- list.files("Output", recursive = TRUE, full.names = TRUE)
awot_files <- files[grepl("awot.rds", files)]
awt_files <- files[grepl("awt.rds", files)]

# Load the pipe operator.
library(magrittr)

# Merge summarized results for the without time summary.
try({file.remove(file.path(cacheDir, "MergeLog_wo_t.txt"))})
awot <- parallel::parLapply(
  cl = parallel::makeCluster(
    parallel::detectCores(), 
    outfile = file.path(cacheDir, "MergeLog_wo_t.txt")
  ),
  awot_files,
  function(filename){
    # Load file from disk.
    data_output <- readRDS(filename)
    
    # Return the object.
    return(data_output)
  }
) %>%
  dplyr::bind_rows()
saveRDS(awot, file.path(cacheDir, "awot.rds"))


# Merge summarized results for the with time summary.
try({file.remove(file.path(cacheDir, "MergeLog_w_t.txt"))})
awt <- parallel::parLapply(
  cl = parallel::makeCluster(
    parallel::detectCores(), 
    outfile = file.path(cacheDir, "MergeLog_w_t.txt")
  ),
  awt_files,
  function(filename){
    # Load file from disk.
    data_output <- readRDS(filename)
    
    # Return the object.
    return(data_output)
  }
) %>%
  dplyr::bind_rows()
saveRDS(awt, file.path(cacheDir, "awt.rds"))
