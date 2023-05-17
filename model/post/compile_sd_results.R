# --------------
#' Date: 2020-07-10
# --------------

Sys.umask(mode = 002)
os <- .Platform$OS.type
library(data.table)

# ----- SET PARAMS 
outdir = "FILEPATH/"

## Demographics
source(paste0("FILEPATH/get_location_metadata.R"))
loc_meta <- get_location_metadata(location_set_id=9,release_id=9)
loc_meta <- loc_meta[is_estimate == 1 & most_detailed == 1]
location_ids <- unique(loc_meta$location_id)


# ----- Launch job
param_map <- data.table(expand.grid(
  
  location_id=location_ids
  
))


param_map_filepath <- file.path("FILEPATH/param_map.csv")
write.csv(param_map, param_map_filepath, row.names = F, na = "")

## Run below to check for missing and relaunch
files <- list.files("FILEPATH")
files <- gsub(".csv","",files)
files <- as.integer(files)
files_want <- param_map$location_id
files_want <- files_want[!(files_want %in% files)]

param_map <- data.table(expand.grid(
  
  location_id=files_want
  
))

write.csv(param_map, param_map_filepath, row.names = F, na = "")

## Return here

## SBATCH Command
job_name <- paste0("compile_anemia_ens_var")
thread_flag <- "-c 4"
mem_flag <- "--mem=25G"
runtime_flag <- "-t 01:00:00"
queue_flag <- "-p all.q"
throttle_flag <- "300" 
n_jobs <- paste0("1-", nrow(param_map), "%", throttle_flag) 
script <- paste0("FILEPATH/compile_sd_results_scripts.R")
error_filepath <- paste0("FILEPATH/%x.e%j")
output_filepath <- paste0("FILEPATH/%x.o%j")
project_flag<- "-A PROJECT"

# add jdrive_flag if needed
sbatch_command <- paste( "COMMAND")

system(sbatch_command)

