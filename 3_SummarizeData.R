# Summarize processed data ------------------------------------------------


# Zach Colburn
# May 7, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This script will take the results of processing a subset of PIV data and 
# save the summarized data in two forms, one in which the full time series is 
# considered and one in which it is not. These files are stored in the folder 
# from which the processed data came and are labeled s_out_wo_t.rds and 
# s_out_w_t.rds, respectively.


# Acquire user data -------------------------------------------------------

# Choose the output directory.
svDialogs::dlg_message("Select the output directory that needs processing.")
outputDir <- svDialogs::dlgDir("Output")$res

# Summarize processed data ------------------------------------------------

# Find folders to be summarized.
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

# Summarize processed results.
try({file.remove(file.path(outputDir, "SummarizeLog.txt"))})
adat <- parallel::parLapply(
  cl = parallel::makeCluster(
    parallel::detectCores(), 
    outfile = file.path(outputDir, "SummarizeLog.txt")
  ),
  dirs,
  function(folder){
    # Load the pipe operator.
    library(magrittr)
    
    # Load from disk.
    data_output <- readRDS(file.path(folder, "data.rds"))
    
    # Summarize data.
    d_out <- data_output %>%
      dplyr::select(# Toss unimportant columns.
        -frame, 
        -x, -y, -ux1, -uy1, -p1,
        -x_peg, -y_peg,
        -background_gfp,
        -xv_neighbor, -yv_neighbor
      )
    d_out$angle_bin <- cut(d_out$angle, breaks = c(-Inf,seq(pi/4,pi,pi/4)))
    d_out$cos_bin <- cut(d_out$cos_similarity, breaks = c(-Inf,seq(-0.8,1,0.2)))
    
    s_out_wo_t <- d_out %>%
      dplyr::group_by(
        positiveCell, negativeCell, posCellProp, negCellProp, folder,# Obligatory groupings.
        neighbor_time_fwd, band_low, band_high,# Group by band and forward time.
        angle_bin, cos_bin# Group by angle_bin and cos_bin.
      ) %>%
      dplyr::filter(!is.na(cos_bin), !is.na(angle_bin)) %>%
      dplyr::summarise(
        PROP = n()/nrow(d_out),
        MEAN_c_gfp = mean(gfp),
        MEAN_n_gfp = mean(gfp_neighbor),
        MEDIAN_c_gfp = median(gfp),
        MEDIAN_n_gfp = median(gfp_neighbor)
      )
    s_out_w_t <- d_out %>%
      dplyr::group_by(
        positiveCell, negativeCell, posCellProp, negCellProp, folder,# Obligatory groupings.
        neighbor_time_fwd, band_low, band_high,# Group by band and forward time.
        angle_bin, cos_bin,# Group by angle_bin and cos_bin.
        time# group by time.
      ) %>%
      dplyr::filter(!is.na(cos_bin), !is.na(angle_bin)) %>%
      dplyr::summarise(
        PROP = n()/nrow(d_out),
        MEAN_c_gfp = mean(gfp),
        MEAN_n_gfp = mean(gfp_neighbor),
        MEDIAN_c_gfp = median(gfp),
        MEDIAN_n_gfp = median(gfp_neighbor)
      )
    
    # Write summarized data to rds files.
    saveRDS(s_out_wo_t, file.path(folder, "s_out_wo_t.rds"))
    saveRDS(s_out_w_t, file.path(folder, "s_out_w_t.rds"))
    
    # Print folder name to the log.
    print(folder)
  }
)
