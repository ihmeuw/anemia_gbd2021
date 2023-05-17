rm(list=ls())

Sys.umask(mode = 002)

library(data.table)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
param_map_filepath <- args[1]

## Retrieving array task_id
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
param_map <- fread(param_map_filepath)

loc_id = param_map[task_id, location_id]

print(loc_id)


# Work
setwd("FILEPATH")
files <- list.files()
files <- files[files %like% paste0("meanSD_",loc_id,"_")]
print(paste0("Number of files is",length(files)))

dt <- lapply(files,readRDS) %>% rbindlist()
dt[,measure_id:=19]
print("Files read in - writing out!")
write.csv(dt,paste0("FILEPATH/",loc_id,".csv"),row.names=F)

