# Analyze the merged summarized data in the cache folder ------------------


# Zach Colburn
# May 7, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This script will call the analysis scripts located in the 'Scripts' folder.


# Acquire user data -------------------------------------------------------

# Choose the analysis directory.
svDialogs::dlg_message("Select the analysis directory.")
analysisDir <- svDialogs::dlgDir("Output")$res
setwd(analysisDir)


# Create the Results folder -----------------------------------------------

if(dir.exists("Results")){unlink("Results", recursive = TRUE, force = TRUE)}
dir.create("Results")
cacheDir <- file.path("Cache")
resutlsDir <- file.path("Results")


# Call analysis scripts ---------------------------------------------------

source(file.path("Scripts", "6_Analyze_wo_time.R"))
source(file.path("Scripts", "6_Analyze_w_time.R"))
