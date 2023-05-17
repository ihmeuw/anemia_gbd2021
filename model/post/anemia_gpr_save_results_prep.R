########################################################################################################################
## Date: 07/24/2020
## Purpose: Prep anemia ST-GPR outputs for save results and plotting
########################################################################################################################
# REQUIRED INPUTS:
# me_name     <- me group or name you want to prep 
#                (can be entire group ("anemia") or specific me (i.e. "anemia_mod_sev"))
########################################################################################################################

########################################################################################################################
################################################# SET-UP ###############################################################
########################################################################################################################

## Clear workspace and load libraries
os <- .Platform$OS.type
if (os == "windows") {
  j <- "FILEPATH"
  h <- "FILEPATH"
} else {
  j <- "FILEPATH"
  user <- Sys.info()[["user"]]
  h <- paste0("FILEPATH", user)
}

## Load functions
library(data.table)
library(dplyr)
library(parallel)
library(DBI)
library(boot)

## Set Date
date  <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d") # this should match the date of ST-GPR modeling

## Source
source("/FILEPATH/utility.r")

########################################################################################################################
############################################ DEFINE FUNCTIONS ##########################################################
########################################################################################################################

## Best run
best.run_id <- function(me) {
  df <- fread("FILEPATH/hemoglobin_run_log.csv")
  run_id <- df[me_name==me & is_best==1]$run_id
  if (length(run_id) > 1) stop("More than 1 run_id")
  return(run_id)
}
## Read draws
read.draws <- function(run_id) {
  path <- paste0("FILEPATH")
  files <- list.files(path, full.names=TRUE)
  if (length(files)==0) stop("No draws")
  df <- lapply(files, fread) %>% rbindlist(., use.names=TRUE)
  
  return(df)
}
## Get mean/lower/upper of draws from DB
pull.collapsed <- function(run.id) {
  df <- model_load(run.id,'raked')
  setnames(df,c("gpr_mean","gpr_lower","gpr_upper"),c("mean_value","lower_value","upper_value"))
  return(df)
}
## Save draws
save.draws <- function(df, me) {
  if (me=="hemoglobin") meid <- 10487
  if (me=="anemia_total") meid <- 25318
  if (me=="anemia_mod_sev") meid <- 25322
  if (me=="anemia_sev") meid <- 25321
  path <- paste0("FILEPATH/",meid)
  unlink(path, recursive=TRUE)
  dir.create(path, showWarnings=TRUE)
  location_ids <- unique(df$location_id)
  mclapply(location_ids, function(x) {
    saveRDS(df[location_id==x], paste0(path, "/", x, ".rds"))
  }, mc.cores=10)
}
## Save collapsed
save.collapsed <- function(df, me, id) {
  prior <- model_load(id,'stage1')[,.(location_id,year_id,sex_id,age_group_id,stage1)]
  if (!(me %in% c("hemoglobin","hemoglobin_pregnant"))) prior$stage1 <- inv.logit(prior$stage1) else prior$stage1 <- exp(prior$stage1)
  st <- model_load(id,'st')[,.(location_id,year_id,sex_id,age_group_id,st)]
  if (!(me %in% c("hemoglobin","hemoglobin_pregnant"))) st$st <- inv.logit(st$st) else st$st <- exp(st$st)
  gpr <- model_load(id,'gpr')[,.(location_id,year_id,sex_id,age_group_id,gpr_mean)]
  setnames(gpr,"gpr_mean","gpr_mean_unraked")
  df <- merge(df,prior,by=c("location_id","year_id","sex_id","age_group_id"),all.x=T)
  df <- merge(df,st,by=c("location_id","year_id","sex_id","age_group_id"),all.x=T)
  df <- merge(df,gpr,by=c("location_id","year_id","sex_id","age_group_id"),all.x=T)
  path <- paste0("FILEPATH/", me, '.rds')
  saveRDS(df, path)
}
## SETUP
prep.draws <- function(me) {
  print("Pulling run ID")
  id <- best.run_id(me)
  print("Reading draws")
  df <- read.draws(id)
  df <- df[, run_id := id]
  print("Saving draws")
  save.draws(df, me)
  print(paste0("Saved draws ", me, " under run_id ", id))
  df <- pull.collapsed(id)
  df <- df[, me_name := me]
  save.collapsed(df, me, id)
  print(paste0("Saved collapsed ", me, " under run_id ", id))
  rm(df)
}

########################################################################################################################
############################################ SAVE RESULTS PREP #########################################################
########################################################################################################################

save_results_prep <- function(me_name) {
  ## Set run log
  run_log <- paste0("FILEPATH/hemoglobin_run_log.csv")
  ## Determine indicators to prep
  if (me_name=="anemia"){ indics <- c("hemoglobin","anemia_total","anemia_mod_sev","anemia_sev")
  } else indics <- me_name
  ## Launch prep
  lapply(indics, function(x) prep.draws(x))
}

## END