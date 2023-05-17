###########################################################
### Date: 2016
### Project: ubCov
### Purpose: Collapse ubcov extraction output
###########################################################

rm(list=ls())
os <- .Platform$OS.type
if (os == "windows") {
  j <- "FILEPATH"
  h <- "FILEPATH"
  ## Load Packages
  pacman::p_load(data.table, haven, dplyr, survey)
} else {
  j <- "FILEPATH"
  user <- Sys.info()[["user"]]
  h <- paste0("FILEPATH/", user, "/")
}

###################
### Setting up ####
###################
## Load Functions
ubcov_central <- "FILEPATH"
setwd(ubcov_central)
source("FILEPATH/launch.r")

######################################################################################################################

## Settings
topic <- "anemia" ## Subset config.csv
config.path <-  "/FILEPATH/config.csv" ## Path to config.csv
parallel <- TRUE ## Run in parallel?
cluster_project <- 'PROJECT' ## You must enter a cluster project in order to run in parallel
fthreads <- 8 ## How many threads per job (used in mclapply) | Set to 1 if running on desktop
m_mem_free <- 15 ## How many GBs of RAM to use per job
h_rt <- "00:45:00"  ## How much run time to request per job | format is HH:MM:SS
logs <- paste0("/FILEPATH/") ## Path to logs

## Launch collapse

df <- collapse.launch(topic=topic, config.path=config.path, parallel=parallel, cluster_project=cluster_project, fthreads=fthreads, m_mem_free=m_mem_free, h_rt=h_rt, logs=logs, central.root=ubcov_central)

