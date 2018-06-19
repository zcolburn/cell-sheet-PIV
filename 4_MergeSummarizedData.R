# Merge summarized data ---------------------------------------------------


# Zach Colburn
# May 7, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This script will take the summarized files, s_out_wo_t.rds and 
# s_out_w_t.rds, and generate an experiment level file containing the merged 
# data from all corresponding summary files. The output files are awot.rds and 
# awt.rds.


# Acquire user data -------------------------------------------------------

# Choose the output directory.
svDialogs::dlg_message("Select the output directory that needs processing.")
outputDir <- svDialogs::dlgDir("Output")$res

# Merge summarized data ---------------------------------------------------

# Find folders to merge.
dirs <- list.files(outputDir, recursive = TRUE, full.names = TRUE)
dirs <- dirs[!grepl("MetaData", dirs)]
outputDirDepth <- length(unlist(strsplit(outputDir, "/")))
dirs <- strsplit(dirs, "/")
dirs <- dirs[!unlist(lapply(dirs, function(i){length(i) <= outputDirDepth+1}))]
dirs <- lapply(dirs, function(i){paste(i[1:(length(i)-1)], collapse = "/")})
dirs <- unlist(dirs)
dirs <- unique(dirs)

# Load the pipe operator.
library(magrittr)

# Merge summarized results for the without time summary.
try({file.remove(file.path(outputDir, "MergeLog_wo_t.txt"))})
awot <- parallel::parLapply(
  cl = parallel::makeCluster(
    parallel::detectCores(), 
    outfile = file.path(outputDir, "MergeLog_wo_t.txt")
  ),
  dirs,
  function(folder){
    # Load from disk.
    data_output <- readRDS(file.path(folder, "s_out_wo_t.rds"))
    
    # Return the object.
    return(data_output)
  }
) %>%
  dplyr::bind_rows()
saveRDS(awot, file.path(outputDir, "awot.rds"))


# Merge summarized results for the with time summary.
try({file.remove(file.path(outputDir, "MergeLog_w_t.txt"))})
awt <- parallel::parLapply(
  cl = parallel::makeCluster(
    parallel::detectCores(), 
    outfile = file.path(outputDir, "MergeLog_w_t.txt")
  ),
  dirs,
  function(folder){
    # Load from disk.
    data_output <- readRDS(file.path(folder, "s_out_w_t.rds"))
    
    # Return the object.
    return(data_output)
  }
) %>%
  dplyr::bind_rows()
saveRDS(awt, file.path(outputDir, "awt.rds"))
