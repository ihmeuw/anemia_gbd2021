###########################################################################################################
### Date: 03/23/2020
### Project: ST-GPR
### Purpose: Parallel launch custom first stage prior testing and assemble for upload to ST-GPR 
###########################################################################################################

######################################
############### SET-UP ###############
######################################

rm(list=ls())
Sys.umask(mode = 002)
os <- .Platform$OS.type
if (os=="windows") {
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
} else {
  lib_path <- "FILEPATH"
  username <- Sys.info()[["user"]]
  j <- "FILEPATH"
  h <- paste0("FILEPATH",username)
  k <- "FILEPATH"
}

pacman::p_load(data.table,magrittr)

######################################
################ ARGS ################
######################################
release_id = 10

## First create param_map
param_map <- fread("FILEPATH/paths.csv")
param_map <- param_map[!is.na(modelable_entity_id) & !(me_group %like% "pregnant")] 
param_map[, release:=release_id]
param_map_filepath <- "FILEPATH/stage1_params.csv"
write.csv(param_map,param_map_filepath,row.names=F)


## QSUB Command
job_name <- "anemia_ensemble_stage1"
thread_flag <- "-c 5"
mem_flag <- "--mem=10G"
runtime_flag <- "-t 6:00:00"
jdrive_flag <- "-C archive"
queue_flag <- "-p all.q"
n_jobs <- paste0("1-", nrow(param_map),"%50")
next_script <- "FILEPATH/launch_prior_parallel_script.R"
error_filepath <- paste0("FILEPATH/%x.e%j")
output_filepath <- paste0("FILEPATH/%x.o%j")
project_flag<- "-A PROJECT"

# add jdrive_flag if needed
qsub_command <- paste( "COMMAND")

system(qsub_command)
