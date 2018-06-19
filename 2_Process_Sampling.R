# Process data generated from the PIV plugin ------------------------------


# Zach Colburn
# May 7, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This script will take the aggregated data generated using the PIV plugin, 
# subset it, and then save the results for that subset in the folder from which 
# the data came.


# Acquire user data -------------------------------------------------------

# Choose the output directory.
svDialogs::dlg_message("Select the output directory that needs processing.")
outputDir <- svDialogs::dlgDir("Output")$res

# Select the maximum number of central pegs to analyze.
invalid <- TRUE
while(invalid){
  max_central_pegs <- svDialogs::dlg_input(
    message = "Maximum number of central pegs to analyze", default = 100
  )$res
  invalid <- !grepl("^[0-9]+$", max_central_pegs) || (max_central_pegs <= 0)
}
# Select the maximum number of 'neighbor' pegs to analyze.
invalid <- TRUE
while(invalid){
  max_neighbors <- svDialogs::dlg_input(
    message = "Maximum number of 'neighbor' pegs to analyze", default = 20
  )$res
  invalid <- !grepl("^[0-9]+$", max_neighbors) || (max_neighbors <= 0)
}
# Select the maximum amount of time forward to analyze.
invalid <- TRUE
while(invalid){
  max_t_forward <- svDialogs::dlg_input(
    message = "Width of temporal analysis frame (s)", default = 1200
  )$res
  invalid <- !grepl("^[0-9]+$", max_t_forward) || (max_t_forward <= 0)
}
# Select the maximum analysis radius.
invalid <- TRUE
while(invalid){
  max_distance_microns <- svDialogs::dlg_input(
    message = "Maximum analysis radius (microns)", default = 180
  )$res
  invalid <- !grepl(
    "^[0-9]+$", 
    max_distance_microns) || (max_distance_microns <= 0
  )
}
# Select the analysis band width.
invalid <- TRUE
while(invalid){
  dist_interval_microns <- svDialogs::dlg_input(
    message = "Width of analysis bands (microns)", default = 60
  )$res
  invalid <- !grepl("^[0-9]+$", dist_interval_microns) || (dist_interval_microns <= 0)
}
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

# Declare any relevant notes about the data
notes <- svDialogs::dlg_input(
  message = "Notes", 
  default = paste0("Time run: ", Sys.time())
)$res


# Output a file containing the data processing parameters.
proc_param <- list(
  "Maximum number of central pegs to analyze [max_central_pegs]: " = max_central_pegs,
  "Maximum number of 'neighbor' pegs to analyze [max_neighbors]: " = max_neighbors,
  "Width of temporal analysis frame (s) [max_t_forward]: " = max_t_forward,
  "Maximum analysis radius (microns) [max_distance_microns]: " = max_distance_microns,
  "Width of analysis bands (microns) [dist_interval_microns]: " = dist_interval_microns,
  "Number of cores used [num_cores]: " = num_cores,
  "Notes [notes]: " = notes
)
parametersOut <- data.frame(
  "Col1" = names(proc_param),
  "Col2" = unlist(proc_param, use.names = FALSE)
)
file_Processing <- file.path(outputDir, "Processing.txt")
write.table(
  tibble::as_tibble(parametersOut),
  file.path(file_Processing),
  row.names = FALSE, col.names = FALSE,
  quote = FALSE
)


# Find folders to be processed.
dirs <- list.files(outputDir, recursive = TRUE, full.names = TRUE)
dirs <- dirs[!grepl("MetaData", dirs)]
outputDirDepth <- length(unlist(strsplit(outputDir, "/")))
dirs <- strsplit(dirs, "/")
dirs <- dirs[!unlist(lapply(dirs, function(i){length(i) <= outputDirDepth+1}))]
dirs <- lapply(dirs, function(i){paste(i[1:(length(i)-1)], collapse = "/")})
dirs <- unlist(dirs)
dirs <- unique(dirs)



# Process data ------------------------------------------------------------

iterationData <- data.frame(
  "experimentFile" = file.path(outputDir, "Experiment.txt"),
  "folder" = dirs,
  "processingFile" = file_Processing,
  "id" = 1:length(dirs),
  stringsAsFactors = FALSE
)

# Temporarilty convert raw.tif files to rds objects. This is done because the 
# function ijtiff::read_tif uses ~10 GB to load each raw.tif file 
# (1040x1392x2x66). Thus, running this step in parallel with more than 2 cores 
# is unfeasible. Below, this task is implemented with a single core but on 
# machines with more than ~20 GB of RAM this could be increased.
preproc_num_cores <- 2
if(parallel::detectCores() < preproc_num_cores){
  preproc_num_cores <- parallel::detectCores()
}
try({file.remove(file.path(outputDir, "ProcessingImageLog.txt"))})
parallel::parLapply(
  cl = parallel::makeCluster(
    preproc_num_cores, 
    outfile = file.path(outputDir, "ProcessingImageLog.txt")
  ),
  split(iterationData, 1:nrow(iterationData)),
  function(c_iteration){
    # Get folder and Experiment file paths.
    folder <- c_iteration$folder
    file_experiment <- c_iteration$experimentFile
    
    # Get experiment data.
    source(file.path("Scripts", "ReadExperimentFile.R"))
    parameters <- ReadExperimentFile(file_experiment)
    
    # Retrieve the raw.tif file to get its fluorescence data.
    img_raw <- ijtiff::read_tif(
      file.path(folder, "raw.tif")
    )
    
    # Extract only the GFP channel.
    img_raw <- img_raw[,,parameters$ch_GFP,]
    
    # Convert the values from numeric to integer because this will reduce the 
    # object's size by ~50% and make it take less time to save to disk.
    img_raw <- array(as.integer(img_raw), dim = dim(img_raw))
    
    # Save to disk.
    saveRDS(img_raw, file.path(folder, "img_raw.rds"))
    
    # Send output to log file.
    print(folder)
  }
)


try({file.remove(file.path(outputDir, "ProcessingLog.txt"))})
parallel::parLapply(
  cl = parallel::makeCluster(
    num_cores,
    outfile = file.path(outputDir, "ProcessingLog.txt")
  ),
  split(iterationData, 1:nrow(iterationData)),
  function(c_iteration){
    # Load the pipe operator.
    library(magrittr)
    
    # Get folder, experiment, and processing paths.
    folder <- c_iteration$folder
    file_experiment <- c_iteration$experimentFile
    file_Processing <- c_iteration$processingFile
    
    # Get PIV data.
    dat <- readr::read_csv(file.path(folder, "PIV.txt"))
    
    # Get experiment data.
    source(file.path("Scripts", "ReadExperimentFile.R"))
    parameters <- ReadExperimentFile(file_experiment)
    
    # Get processing data.
    source(file.path("Scripts", "ReadProcessingFile.R"))
    proc_param <- ReadProcessingFile(file_Processing)
    max_central_pegs <- proc_param$max_central_pegs
    max_neighbors <- proc_param$max_neighbors
    max_t_forward <- proc_param$max_t_forward
    max_distance_microns <- proc_param$max_distance_microns
    dist_interval_microns <- proc_param$dist_interval_microns
    
    # Determine experimental parameters.
    frames <- sort(unique(dat$frame))
    frames_N <- length(frames)
    times <- sort(unique(dat$time))
    
    # Invert the y-axis so that it matches the TIFF files.
    dat$y <- parameters$height-dat$y+1
    
    # Create a list of matrices to hold data for each frame.
    pegs_x <- sort(unique(dat$x))
    pegs_N_x <- length(pegs_x)
    pegs_y <- sort(unique(dat$y))
    pegs_N_y <- length(pegs_y)
    
    t_x <- rep(
      list(matrix(NA, nrow = pegs_N_y, ncol = pegs_N_x)),
      frames_N
    )
    names(t_x) <- frames
    t_y <- t_x
    t_gfp <- t_x
    
    # Retrieve the raw.tif file to get its fluorescence data.
    img_raw <- readRDS(file.path(folder, "img_raw.rds"))
    
    # Specify the maximum distance to analyze and the distance interval.
    microns_per_pixel <- parameters$microns_in_x/parameters$width
    max_dist_px <- max_distance_microns/microns_per_pixel
    dist_interval_px <- dist_interval_microns/microns_per_pixel
    
    pixels_per_peg <- diff(sort(unique(dat$x))[1:2])
    max_dist_pegs <- max_dist_px/pixels_per_peg
    dist_interval_pegs <- dist_interval_px/pixels_per_peg
    
    dist_bands <- seq(0, ceiling(max_dist_pegs), by = dist_interval_pegs)
    
    real_dists_microns <- dist_bands*pixels_per_peg*microns_per_pixel
    
    # Make masks.
    mask_indices <- rep(list(list()), length(dist_bands)-1)
    peg_sep <- dist_interval_pegs/2
    for(i in 2:length(dist_bands)){
      limit <- ceiling(dist_bands[i])
      dims <- 2*limit+1
      mask_x <- matrix(-limit:limit, nrow = dims, ncol = dims)
      mask_y <- t(mask_x)
      mask <- sqrt(mask_x^2+mask_y^2)
      mask <- mask > dist_bands[i-1] & mask <= dist_bands[i]
      indices <- which(mask, arr.ind = TRUE)
      indices <- matrix(c(mask_y[indices],mask_x[indices]), ncol = 2)
      mask_indices[[i-1]] <- indices
    }
    
    # Insert peg columns into dat.
    dat <- dplyr::mutate(
      dat,
      x_peg = as.integer((x - min(x))/pixels_per_peg+1),
      y_peg = as.integer((y - min(y))/pixels_per_peg+1)
    )
    
    # Split dat based on frame.
    dat <- split(dat, f = factor(dat$frame))
    names(dat) <- frames
    
    # Insert GFP data into dat then fill the t_ objects.
    for(i in as.character(frames)){
      indices <- as.matrix(dplyr::select(dat[[i]], y, x))
      gfp_slice <- img_raw[,,as.integer(i)]
      dat[[i]] <- dplyr::mutate(
        dat[[i]],
        gfp = gfp_slice[indices]
      )
      
      # Determine peg indices from x and y values.
      peg_indices <- as.matrix(dplyr::select(dat[[i]], y_peg, x_peg))
      
      # Insert known values.
      t_x[[i]][peg_indices] <- dat[[i]]$ux1
      t_y[[i]][peg_indices] <- dat[[i]]$uy1
      t_gfp[[i]][peg_indices] <- dat[[i]]$gfp
    }
    rm(img_raw)# Remove the raw image object to free up memory.
    
    # Create a list to hold shift correlation at different distances data.
    dist_cors <- rep(
      list(matrix(NA, nrow = pegs_N_y, ncol = pegs_N_x)), 
      max_dist_pegs
    )
    x_plane <- matrix(1:pegs_N_x, nrow = pegs_N_y, pegs_N_x, byrow = TRUE)
    y_plane <- matrix(
      1:pegs_N_y, nrow = pegs_N_y, pegs_N_x, byrow = FALSE
    )[pegs_N_y:1,]
    flush.console()
    
    # Set the maximum number of sampled central pegs parameter.
    num_central_pegs <- nrow(dat[[1]])
    if(num_central_pegs < max_central_pegs){
      best_central_pegs <- num_central_pegs
    }else{
      best_central_pegs <- max_central_pegs
    }
    
    # Set the maximum amount of time forward to evaluate a vector's influence.
    max_frames_forward <- floor(max_t_forward/parameters$interval)
    max_analysis_frame <- tail(frames, 1) - max_frames_forward - 1
    reduced_frames <- 2:max_analysis_frame
    
    max_data_output_rows <- max_neighbors*
      max_central_pegs*
      (max_frames_forward+1)*
      (length(dist_bands)-1)*
      (length(reduced_frames))
    
    for(band_number in 1:(length(dist_bands)-1)){
      peg_shifts <- tibble::as_tibble(mask_indices[[band_number]])
      current_neighbors <- nrow(peg_shifts)
      if(current_neighbors < max_neighbors){
        best_neighbors <- current_neighbors
      }else{
        best_neighbors <- max_neighbors
      }
      
      neighbor_indices <- lapply(
        1:(best_central_pegs),
        function(item){
          dplyr::sample_n(peg_shifts, best_neighbors)
        }
      ) %>%
        dplyr::bind_rows()
      
      # Create shifters.
      ni_y <- matrix(neighbor_indices$V1, ncol = best_neighbors, byrow = TRUE)
      ni_x <- matrix(neighbor_indices$V2, ncol = best_neighbors, byrow = TRUE)
      
      flush.console()
      for(i in as.character(reduced_frames)){
        flush.console()
        text <- sprintf("band %s, frame %s, time %s", band_number, i, Sys.time())
        # print(text)
        flush.console()
        
        # Select the brightest central pegs.
        small_dat <- dat[[i]]
        small_dat <- dplyr::filter(
          small_dat,
          x_peg > dist_bands[band_number+1] + peg_sep,
          x_peg < pegs_N_x - dist_bands[band_number+1] - ceiling(peg_sep),
          y_peg > dist_bands[band_number+1] + peg_sep,
          y_peg < pegs_N_y - dist_bands[band_number+1] - ceiling(peg_sep)
        )
        small_dat <- small_dat %>%
          dplyr::arrange(., dplyr::desc(gfp))
        small_dat <- head(small_dat, best_central_pegs)
        small_dat$peg_id <- 1:nrow(small_dat)
        
        # Get shifts.
        x_shifts <- ni_x + small_dat$x_peg
        y_shifts <- ni_y + small_dat$y_peg
        
        shifts <- matrix(c(ni_y, ni_x), ncol = 2)
        
        indices <- matrix(c(y_shifts, x_shifts), ncol = 2)
        
        remapper <- tibble::data_frame(
          peg_id = rep(1:nrow(small_dat), (max_frames_forward+1)*ncol(x_shifts)),
          frame = as.integer(i)
        )
        
        # Get vector components and gfp levels at neighbors.
        xv <- matrix(NA, nrow = nrow(indices), ncol = max_frames_forward+1)
        yv <- xv
        gfp <- xv
        for(j in 0:max_frames_forward){
          set_frame <- as.character(j+as.integer(i))
          vc_row_indices <- (j*best_central_pegs+1):((j+1)*best_central_pegs)
          xv[,j+1] <- t_x[[as.integer(i)+j]][indices]
          yv[,j+1] <- t_y[[as.integer(i)+j]][indices]
          gfp[,j+1] <- t_gfp[[as.integer(i)+j]][indices]
        }
        remapper$xv_neighbor <- as.vector(xv)
        remapper$yv_neighbor <- as.vector(yv)
        remapper$gfp_neighbor <- as.vector(gfp)
        remapper$neighbor_frame_fwd <- rep(
          0:max_frames_forward, 
          each = nrow(indices)
        )
        
        # Insert neighbor vector components into small_dat in the appropriate 
        # location.
        small_dat <- dplyr::inner_join(
          small_dat,
          remapper,
          by = c("peg_id","frame")
        )
        
        # Add neighbor id numbers to small_dat.
        small_dat$neighbor_id <- rep(
          1:best_neighbors, 
          nrow(small_dat)/best_neighbors
        )
        
        # Get matrices used to calculate correlations.
        central_vectors <- matrix(c(small_dat$ux1, small_dat$uy1), ncol = 2)
        neighbor_vectors <- matrix(
          c(small_dat$xv_neighbor, small_dat$yv_neighbor), ncol = 2
        )
        valid_rows <- is.na(neighbor_vectors[,1]) | is.na(central_vectors[,1])
        valid_rows <- !valid_rows
        
        neighbor_vectors <- neighbor_vectors[valid_rows,]
        central_vectors <- central_vectors[valid_rows,]
        
        central_mag <- sqrt(base::rowSums(central_vectors^2))
        neighbor_mag <- sqrt(base::rowSums(neighbor_vectors^2))
        
        # Calculate cosine similarity for central vectors and their neighbors.
        cos_similarity <- base::rowSums(neighbor_vectors*central_vectors)/(
          (neighbor_mag)*(central_mag)
        )
        small_dat$cos_similarity <- cos_similarity
        
        
        # Normalize neighbor shifts to unit length.
        norm_shifts <- shifts/sqrt(base::rowSums(shifts^2))
        neighbor_sep <- rep(
          list(tibble::as_data_frame(norm_shifts)), 
          max_frames_forward+1
        ) %>% 
          dplyr::bind_rows() %>%
          as.matrix()
        
        # Normalize central vectors to unit length.
        norm_cvs <- central_vectors/sqrt(base::rowSums(central_vectors^2))
        
        # Calculate angle between central vector and neighbor position relative to 
        # the central peg.
        angles <- acos(
          base::rowSums(neighbor_sep*norm_cvs)/
            sqrt(base::rowSums(neighbor_sep*neighbor_sep)*
                   base::rowSums(norm_cvs*norm_cvs))
        )
        small_dat$angle <- angles
        
        # Determine cell types and estimate background fluorescence.
        terminal_folderName <- gsub(".*/","",folder)
        nc <- parameters$null_cell
        pc <- parameters$plus_cell
        
        posCell <- NA
        posCellProp <- NA
        negCell <- NA
        negCellProp <- NA
        for(ncj in nc){
          for(pcj in pc){
            if(grepl(paste0("^",ncj,"-"), terminal_folderName)){
              negCell <- ncj
              background_gfp_level <- max(small_dat$gfp) + 1
            }else if(grepl(paste0("^",pcj,"-"), terminal_folderName)){
              posCell <- pcj
              background_gfp_level <- min(small_dat$gfp) - 1
            }else if(
              grepl(
                paste0("^",ncj,"_","[[:digit:]]+_to_",pcj,"_","[[:digit:]]+-"),
                terminal_folderName
              )
            ){
              split_folderName <- unlist(strsplit(terminal_folderName, "-"))[1]
              split_folderName <- unlist(strsplit(split_folderName, "_"))
              
              negCell <- ncj
              negCellProp <- as.numeric(split_folderName[2])
              posCell <- pcj
              posCellProp <- as.numeric(split_folderName[5])
              
              background_gfp_level <- quantile(
                small_dat$gfp, c(posCellProp/(posCellProp+negCellProp))
              )
            }else if(
              grepl(
                paste0("^",pcj,"_","[[:digit:]]+_to_",ncj,"_","[[:digit:]]+-"),
                terminal_folderName
              )
            ){
              split_folderName <- unlist(strsplit(terminal_folderName, "-"))[1]
              split_folderName <- unlist(strsplit(split_folderName, "_"))
              
              negCell <- ncj
              negCellProp <- as.numeric(split_folderName[5])
              posCell <- pcj
              posCellProp <- as.numeric(split_folderName[2])
              
              background_gfp_level <- quantile(
                small_dat$gfp, c(posCellProp/(posCellProp+negCellProp))
              )
            }
          }
        }
        
        # Add experiment data to small_dat.
        small_dat <- dplyr::mutate(
          small_dat,
          background_gfp = background_gfp_level,
          positiveCell = posCell,
          posCellProp = posCellProp,
          negativeCell = negCell,
          negCellProp = negCellProp,
          folder = folder,
          neighbor_frame_fwd = neighbor_frame_fwd*parameters$interval
        ) %>%
          dplyr::rename(neighbor_time_fwd = "neighbor_frame_fwd")
        
        
        # library(plotly)
        # plot_ly(
        #   small_dat %>%
        #     dplyr::filter(
        #       #angle > pi*1/8,
        #       #angle < pi*2/8
        #       # central_mag > 4,
        #       # neighbor_mag > 4
        #       TRUE
        #     ) %>%
        #     dplyr::mutate(
        #       positive = factor(gfp > background_gfp_level)
        #     ) %>%
        #     dplyr::group_by(positive) %>%
        #     dplyr::do(., dplyr::sample_n(.,10000)),
        #   type = "scatter3d", mode = "markers",
        #   x = ~gfp, y = ~cos_similarity, z = ~angle,
        #   color = ~positive,
        #   marker = list(size = 2)
        # )
        
        # high_gfp_cos <- dplyr::filter(gfp_cos, central_mag > 10)
        # cor.test(high_gfp_cos$cos_similarity, high_gfp_cos$gfp, method = "pearson")
        
        
        # Append new data to the output object.
        small_dat$band_low <- real_dists_microns[band_number]
        small_dat$band_high <- real_dists_microns[band_number+1]
        if(band_number == 1 & i == as.character(reduced_frames[1])){
          data_output <- small_dat
          data_output[(nrow(small_dat)+1):max_data_output_rows,] <- NA
        }else{
          index_low <- which(is.na(data_output$frame))[1]
          index_high <- index_low + nrow(small_dat) - 1
          data_output[index_low:index_high,] <- small_dat
        }
      }
    }
    
    # Remove empty rows at the bottom.
    data_output <- dplyr::filter(data_output, !is.na(data_output$frame))
    
    # Save the processed data.
    saveRDS(data_output, file.path(folder, "data.rds"))
    
    # Append the name of the folder just analyzed to ProcessingLog.txt.
    print(folder)
  }
)





# Remove the img_raw.rds files created during the pre-processing step above.
parallel::parLapply(
  cl = parallel::makeCluster(1),
  split(iterationData, 1:nrow(iterationData)),
  function(c_iteration){
    # Get folder path.
    folder <- c_iteration$folder
    
    # Delete the file.
    try({file.remove(file.path(folder, "img_raw.rds"))})
  }
)
