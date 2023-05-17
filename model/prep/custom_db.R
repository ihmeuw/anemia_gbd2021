########################################################################################################################
## Date: 07/07/2019
## Purpose: Upload data bundles into custom DB (model type: ST-GPR)
########################################################################################################################

########################################################################################################################
################################################# SET-UP ###############################################################
########################################################################################################################

## System set-up ##
message("Setting up environment...")
username <- Sys.info()[["user"]]
if (Sys.info()["sysname"]=="Linux") {
  j_root <- "FILEPATH" 
  h_root <- file.path("FILEPATH", username)
} else if (Sys.info()["sysname"]=="Darwin") {
  j_root <- "FILEPATH"
  h_root <- "FILEPATH"
} else { 
  j_root <- "FILEPATH"
  h_root <- "FILEPATH"
}

## Load central functions, packages, and reference db
source("FILEPATH/get_bundle_data.R")
source("FILEPATH/upload_bundle_data.R")

########################################################################################################################
############################################ DEFINE FUNCTIONS ##########################################################
########################################################################################################################

## Clear existing epi data in bundle
clear_data <- function(BUNDLE, FOLDER, decomp_step) {
  
  # pull current data in bundle
  data <- get_bundle_data(bundle_id=BUNDLE, decomp_step=decomp_step)[, .(seq)]
  # upload just seq column to clear everything in bundle
  clear_old_path <- file.path(j_root, "FILEPATH", 
                              paste0("clear_old_data_", FOLDER, "_", format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M"), ".xlsx"))
  write.xlsx(data, clear_old_path, row.names=FALSE, col.names=TRUE, sheetName="extraction")
  upload_bundle_data(bundle_id=BUNDLE, filepath=clear_old_path, decomp_step=decomp_step)
  # check to make sure bundle is empty before moving on
  epi_data <- get_bundle_data(bundle_id=BUNDLE, decomp_step=decomp_step)[, .(seq)]
  if (length(epi_data$seq) >= 1) { stop(paste0("STOP | Bundle ", BUNDLE, " not empty for new upload")) 
    } else { print(paste0("Congrats! You've cleared out all data from bundle ", BUNDLE, " from acause ", FOLDER)) }
  
}

## Upload data to epi db
upload_data <- function(BUNDLE, FOLDER, data, decomp_step) {
  new_path <- file.path(j_root, "FILEPATH", 
                        paste0("new_data_", FOLDER, "_", format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M"), ".xlsx"))
  dataset <- copy(data)
  dataset[, data_sheet_file_path := new_path]
  write.xlsx(dataset, new_path, row.names=FALSE, col.names=TRUE, sheetName="extraction")
  upload_bundle_data(bundle_id=BUNDLE, filepath=new_path, decomp_step=decomp_step, gbd_round_id=7)
  print(paste0("Congrats! You've uploaded your data to bundle ", BUNDLE, " from acause ", FOLDER))
}