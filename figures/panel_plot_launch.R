### Anemia Manuscript Panel Plots Launch Script

rm(list=ls())

# Set-up
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/cluster_tools.R")
library(data.table)

# Parameters
locations <- get_location_metadata(91, gbd_round_id=7)
loc_ids <- locations$location_id

param_map <- data.table(location_id=loc_ids)
param_map_filepath <- "FILEPATH/panel_param_map.csv"

write.csv(param_map, param_map_filepath, row.names = F)

temp_dir <- "FILEPATH"
dir.create(temp_dir)

# Launch

## sbatch Command
job_name <- "anemia_panel_plots"  
thread_flag <- "-c 2" 
mem_flag <- "--mem=5G" 
runtime_flag <- "-t 00:05:00"
queue_flag <- "-p all.q" 
throttle_flag <- "300" 
n_jobs <- paste0("1-", nrow(param_map), "%", throttle_flag) 
next_script <- "FILEPATH/panel_plot_calc.R" 
error_filepath <- paste0("-e FILEPATH/%x.e%j")
output_filepath <- paste0("-o FILEPATH/%x.o%j") 
project_flag<- "-A PROJECT"  

sbatch_command <- paste( "COMMAND")
system(sbatch_command)

# Combine

## job hold
file_list <- paste0(temp_dir,"/",loc_ids,".pdf")
job_hold("anemia_p", file_list=file_list) 

### append graph, order by specified sort  
files <- gsub(",", "", toString(paste0(temp_dir, "/", loc_ids, ".pdf")))
outpath <- "FILEPATH/panel_plots.pdf"
append_pdf(files, outpath)

### clean up temp data
unlink(temp_dir, recursive=TRUE)
print(paste0("Graph complete, saved to ", outpath))
