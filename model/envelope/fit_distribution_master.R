###########################################################
### Date: 1/3/2017
### Project: Anemia
### Purpose: Fit Distribution to Modeled Mean & Standard Deviation. Launches parallelization of disriubtion type (ensemble or weibull)
###########################################################

#----CONFIG----------------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

###########################################################
#
# CHOOSE DISTRIBUTION TYPE 
distribution <- "ensemblemv2p"
#
###########################################################


os <- .Platform$OS.type
if (os=="windows") {
  j <- "FILEPATH"
  h <- "FILEPATH"
} 
if (os=="unix") {
  j <- "FILEPATH"
  h <- "FILEPATH"
}

# load packages, install if missing
library(data.table)

# Settings
project <- "-P PROJECT " 
draws.required <- 1000 
resume <- F 
version <- "gbd2020_v3_annual"
release_id <- 9

###Specify Directories###
code.dir <- paste0("FILEPATH")
output_root <- paste0("FILEPATH/",version)

# create directories
dir.create(file.path(output_root), showWarnings = FALSE, recursive = TRUE)

#********************************************************************************************************************************	

#DROP OLD FILES 
if (resume==FALSE){  
  for(meid in c(10489, 10490, 10491, 10507)) {
    print(paste("DROPPING OLD RESULTS FOR MEID", meid))
    dir.create(paste0(output_root, "/", meid), showWarnings = FALSE, recursive=TRUE)
    setwd(paste0(output_root, "/", meid))
    old_files <- list.files()
    file.remove(old_files)
  } 
}


#----LAUNCH JOBS-----------------------------------------------------------------------------------------------------------------
  
source(paste0("FILEPATH/get_location_metadata.R"))
loc_meta <- get_location_metadata(location_set_id=9,gbd_round_id=7)
loc_meta <- loc_meta[is_estimate == 1 & most_detailed == 1]
locs <- loc_meta$location_id
  
# Make param map + write out
param_map <- data.frame(code.dir=code.dir,
                          output_root=output_root,
                          loc=locs,
                          draws.required=draws.required,
                          distribution=distribution,
                          location_id=locs,
                          release_id=release_id)
  
param_map_filepath <- paste0(output_root,"/param_map.csv")
write.csv(param_map, param_map_filepath, row.names=F)
  
  
## SBATCH Command
job_name <- paste0("anemia_",distribution)
thread_flag <- "-c 5"
mem_flag <- "--mem=10G"
runtime_flag <- "-t 01:00:00"
queue_flag <- "-p all.q"
throttle_flag <- "150" 
n_jobs <- paste0("1-", nrow(param_map), "%", throttle_flag) 
script <- paste0(code.dir, "/fit_ensemblemv2p_parallel.R")
error_filepath <- paste0("FILEPATH/%x.e%j")
output_filepath <- paste0("FILEPATH/%x.o%j")
project_flag<- "-A PROJECT"
  
# add jdrive_flag if needed
sbatch_command <- paste( "COMMAND")
  
system(sbatch_command)
  

if(T) {
  print("TIMER TO SEE WHEN ALL JOBS ARE DONE")
  finished <- list.files(paste0(output_root, "/10507"))
  while(length(finished) < length(locs)) {
    finished <- list.files(paste0(output_root, "/10507"))
    print(paste(length(finished), "of", length(locs), "jobs finished"))
    Sys.sleep(60)
  }
  print("CONGRATS - ALL JOBS FINISHED")
}
  