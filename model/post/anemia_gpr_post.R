#############################################################################################################
### Date: 07/24/2020
### Project: Anemia
### Purpose: Process and plot GPR outputs and save results
#############################################################################################################

#----CONFIG-------------------------------------------------------------------------------------------------------------
### set up
# clean workspace
rm(list=ls())
username <- Sys.info()[["user"]]
os <- .Platform$OS.type
if (os == "windows") {
  j_root <- "FILEPATH"
} else {
  j_root <- "FILEPATH"
}
# load libraries
pacman::p_load(data.table,dplyr,parallel,DBI)

# source paths and functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_ids.R")
source("FILEPATH/get_crosswalk_version.R")
source('FILEPATH/utility.r')
source("FILEPATH/collapse_cluster_tools.r")
ref <- fread("FILEPATH/hemoglobin_run_log.csv")
me_db <- fread("FILEPATH/hemoglobin_model_db.csv")
#***********************************************************************************************************************


#----FUNCTIONS----------------------------------------------------------------------------------------------------------
### source save_results functions
source("FILEPATH/anemia_gpr_save_results_prep.R")

### get locations
locations <- get_location_metadata(22, release_id=9)

### set paths
date         <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d") # this should match the date of ST-GPR modeling
results.root <- file.path("FILEPATH", date); if (!dir.exists(results.root)) dir.create(results.root, recursive=TRUE)
data.root    <- file.path("FILEPATH",date); if (!dir.exists(data.root)) dir.create(data.root, recursive=TRUE)
draws.root   <- file.path("FILEPATH",date); if (!dir.exists(draws.root)) dir.create(draws.root, recursive=TRUE)
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### prep data into RDS for plotting code
mes <- c("hemoglobin","anemia_total","anemia_mod_sev","anemia_sev")
data <- data.frame()
for (me in mes){
  cwid <- me_db[me_name==me & is_best==1, crosswalk_version_id]
  df <- get_crosswalk_version(cwid)
  df[,me_name:=me]
  data <- rbind(data,df,fill=TRUE)
}
saveRDS(data,file.path("FILEPATH/",date,"anemia_data.rds"))

### prep and save summary versions for vetting, draws for save_results
save_results_prep("anemia")

### plot!
# FAIR cluster: 
system(paste0("COMMAND"))


### Launch save_results_stgpr
for (me in mes){
  
  # Args
  run_id <- ref[me_name==me & is_best==1, run_id]
  me_id <- NA
  save_results <- "stgpr"
  
  ## Run Settings
  job_name <- paste0("save_results_",save_results,"_anemia_", me)
  script <- "FILEPATH/save_results_hemoglobin_parallel.R"
  fthreads <- 15
  m_mem_free <- 90
  h_rt <- "10:00:00"
  cluster_project <- "PROJECT"
  logs <- "FILEPATH"
  ## Argument
  arguments <- c(run_id, me_id, save_results)
  ## Qsub
  qsub(job_name=job_name, script=script, fthreads=fthreads, m_mem_free=m_mem_free, h_rt=h_rt, cluster_project=cluster_project, logs=logs, arguments=arguments)
  
}

## END