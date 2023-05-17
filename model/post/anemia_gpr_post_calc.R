#############################################################################################################
### Date: 07/24/2020
### Project: Anemia
### Purpose: Create Time-Series plots for anemia ST-GPR models
### Inputs:  output.path      ->  file path to where pdfs should be saved, must be cluster compatible
###          add.regions      ->  logical; if regional data points are to be shown
###          add.outliers     ->  logical; if previously outliered data points are to be shown (here, cv_outlier)
###          add.gbd2019      ->  logical; if you want to compare to GBD 2017 estimates
###          y.axis           ->  logical; if scale of y axis should be 0 to 1 for all countries
#############################################################################################################

#----FUNCTION-------------------------------------------------------------------------------------------------------------
plot_gpr <- function (date,
                      data_date     = "NULL",
                      output.mean,
                      output.total,
                      output.modsev,
                      output.sev,
                      add.regions   = TRUE,
                      add.outliers  = TRUE,
                      y.axis        = TRUE,
                      compare       = "NULL",
                      compare_round = 2019) {
  
  ### make sure using on cluster
  if (Sys.info()[1] == "Windows") stop("Must be run on the cluster, not Windows.")
  
  ### list mes to facet
  mes <- c("hemoglobin","anemia_total","anemia_mod_sev","anemia_sev")

  ### where to get model output
  results_path <- file.path("FILEPATH",  date) 
  
  ### pull data
  if (data_date=="NULL") data_date <- date
  to_model_dir <- paste0("/FILEPATH/", data_date)
  print("read data rds")
  data         <- readRDS(file.path(to_model_dir, "anemia_data.rds"))[,c("ihme_loc_id","level"):=NULL][me_name %in% mes]
  data <- merge(data,locations[,.(location_id,ihme_loc_id)])
  data[,location_id:=NULL]
  data <- data %>%
      .[(!is.na(val) | !is.na(is_outlier)) & survey_name %like% "DHS", source_type := "DHS"] %>%
      .[(!is.na(val) | !is.na(is_outlier)) & survey_name %like% "MICS", source_type := "MICS"] %>%
      .[(!is.na(val) | !is.na(is_outlier)) & survey_name %like% "admin", source_type := "Admin"] %>%
      .[(!is.na(val) | !is.na(is_outlier)) & survey_name=="WHO_WHS", source_type := "WHS"] %>%
      .[(!is.na(val) | !is.na(is_outlier)) & vmnis==1, source_type := "VMNIS"] %>%
      .[(!is.na(val) | !is.na(is_outlier)) & is.na(source_type) & !is.na(nclust), source_type := "Other microdata"] %>%
      .[(!is.na(val) | !is.na(is_outlier)) & (is.na(source_type) | source_type=="" | source_type=="Survey - cross-sectional"), source_type := "Scientific Literature"]
  data         <- merge(data, locations[, .(location_id, ihme_loc_id, level)], by="ihme_loc_id", all.x=TRUE)
  data         <- data[level >= 3]
  vars         <- c("location_id", "year_id", "age_group_id", "sex", "me_name")
  cols         <- c(vars, "val", "level", "variance", "is_outlier", "source_type")
  data         <- data[, cols[cols %in% names(data)], with=FALSE]
  dt           <- lapply(file.path(results_path, paste0(mes, ".rds")), readRDS) %>% rbindlist(., fill=TRUE)
  dt[,sex:=ifelse(sex_id==1,"Male","Female")]
  dt           <- merge(dt, data[!is.na(location_id), ], by=vars, all=TRUE)
  dt           <- dt[me_name %in% mes, ]
  print("Data is ready to go")
  
  ### prep comparison lines
  if (!"compare_version" %in% names(dt) & !is.null(compare) & compare != "NULL") {
    # flag to specify GBD round
    if (is.null(compare_round) | compare_round == "NULL" | !compare_round %in% c(2015, 2016, 2017, 2019, 2020)) {
      stop("Can't compare two versions without specifying 'compare_round', the GBD cycle to pull from (e.g. 2016 or 2017)")
    } else{
    # pull path to results for comparison
    compare_path <- file.path("FILEPATH", compare)
    }
    # get old results
    files <- list.files(compare_path, full.names=TRUE)
    want  <- file.path(compare_path, paste0( mes,".rds"))
    pull  <- want[want %in% files]
    if (length(pull) < 1) stop(paste0("No files to compare in ", compare_path))
    dt_compare <- lapply(pull, readRDS) %>% rbindlist(., fill=TRUE)
    # merge on to dataset
    setnames(dt_compare, c("mean_value", "lower_value", "upper_value"), c("gpr_mean_compare", "gpr_lower_compare", "gpr_upper_compare"))
    dt_compare[,`:=` (sex = ifelse(sex_id==1,"Male","Female"), sex_id=NULL)]
    dt <- merge(dt, dt_compare[, c(vars, "gpr_mean_compare"), with=FALSE], by=vars, all.x=TRUE)
  }

  
  ### add useful id names
  # locations
  check_cols <- c("location_name", "ihme_loc_id", "sort_order", "most_detailed", "region_id", "level_3","level", "region_name")
  merge_cols <- NULL
  for (col in check_cols) if (!(col %in% names(dt))) merge_cols <- c(merge_cols, col)
  if (!is.null(merge_cols)) {
    geo <- get_location_hierarchy(location_set_version_id=location_set_version_id)[, c("location_id", merge_cols), with=FALSE]
    setnames(geo, "level_3", "national_id")
    geo[national_id==location_id, national_id := NA_integer_]
    dt <- merge(dt, geo, by="location_id", all.x=TRUE)
  }
  print("Missing location-related columns merged on")
  # ages
  if (!("age_group_name" %in% names(dt))) {
    ages <- get_ids("age_group")
    dt <- merge(dt, ages, by="age_group_id", all.x=TRUE)
  }
  dt[, age_group_name := factor(age_group_name, 
                                levels = c("Early Neonatal","Late Neonatal","1-5 months",
                                           "6-11 months","12 to 23 months","2 to 4","5 to 9",
                                           "10 to 14","15 to 19","20 to 24","25 to 29","30 to 34",
                                           "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
                                           "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84",
                                           "85 to 89","90 to 94","95 plus","All Ages"))]
  # sexes
  bysex <- ifelse(3 %in% unique(dt$sex_id), 0, 1)
  if (!("sex_name" %in% names(dt))) {
    dt[sex_id==1, sex_name := "Male"]
    dt[sex_id==2, sex_name := "Female"]
    dt[sex_id==3, sex_name := "Both"]
  }
  
  dt[me_name %like% "pregnant", sex_name:="Female, Pregnant"]
  dt[me_name %like% "pregnant", me_name:=gsub("_pregnant","",me_name)]
  print("Prepped with age group and sex names")
  
  ### order by location order
  # if alphabetical, order by ihme_loc_id ABC, otherwise order by region, then alphabetically by ihme_loc_id (sort_order in hierarchy)
  if (abc) { dt <- dt[order(ihme_loc_id) ,] } else { dt <- dt[order(sort_order) ,] }
  print("Dataset sorted")
  
  ### make temp folders for output and save a temp file to be passed to graphing
  data_path <- file.path("FILEPATH/plot_gpr_temp.rds") 
  temp_root <- file.path("FILEPATH/")
  for (me in unique(dt$me_name)){
    dir.create(paste0(temp_root,"/",me), recursive=TRUE, showWarnings=FALSE)
  }
  print("Directories created")
  
  ### keep only locations specified
  locations_nat <- locations[level==3, location_id] %>% unique
  if (national_only) dt <- dt[location_id %in% locations_nat, ]
  
  dt <- dt[location_id %in% locations$location_id]
  dt <- dt[!is.na(year_id)]
  
  ### save dataset for plotting
  saveRDS(dt, file=data_path)
  print(paste0("Dataset saved to data path: ", data_path))
  
  ### launch jobs to graph
  file_list <- NULL
  for (loc in unique(dt$location_id)) { 
    
    file_list <- c(file_list, paste0("FILEPATH",loc,".pdf"))

    # FAIR cluster:
    job <- paste0("COMMAND")
    system(job); print(job)
    
  }
  job_hold(paste0("plot_gpr_anemia_", date, "_", loc), file_list=file_list)
  
  ### append graph by ME, order by specified sort 
  
  ## Mean Hemog
  files <- gsub(",", "", toString(paste0("FILEPATH", unique(dt$location_id), ".pdf")))
  append_pdf(files, output.mean)
  print(paste0("Graph complete, saved to ", output.mean))
  
  ## Total Anemia
  files <- gsub(",", "", toString(paste0("FILEPATH", unique(dt$location_id), ".pdf")))
  append_pdf(files, output.total)
  print(paste0("Graph complete, saved to ", output.total))
  
  ## Mod-Sev Anemia
  files <- gsub(",", "", toString(paste0("FILEPATH", unique(dt$location_id), ".pdf")))
  append_pdf(files, output.modsev)
  print(paste0("Graph complete, saved to ", output.modsev))
  
  ## Sev Anemia
  files <- gsub(",", "", toString(paste0("FILEPATH", unique(dt$location_id), ".pdf")))
  append_pdf(files, output.sev)
  print(paste0("Graph complete, saved to ", output.sev))
  
  ### clean up temp data
  unlink(paste0(temp_root), recursive=TRUE)
  
  ### END OF FUNCTION
  
}
#***********************************************************************************************************************


#----CONFIG-------------------------------------------------------------------------------------------------------------
# args <- commandArgs()[-(1:3)]

username <- Sys.info()[["user"]]
### runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "FILEPATH" 
} else { 
  j_root <- "FILEPATH"
}

### load packages
pacman::p_load(data.table, haven, magrittr, ini, stringr, rhdf5, grid, RMySQL)

### load functions
source("FILEPATH/read_excel.R")
source("FILEPATH/init.r")  
source("FILEPATH/helpers.R")
source("FILEPATH/cluster_tools.r")
source("FILEPATH/ubcov_tools.r")
source("FILEPATH/get_ids.R")
source("FILEPATH/get_location_metadata.R")
locations <- get_location_metadata(location_set_id=22, gbd_round_id=7)
age_ids <- get_ids("age_group")
sex_ids <- get_ids("sex")
gbd_cycle <- "gbd2020"
#***********************************************************************************************************************


#----TIME SERIES--------------------------------------------------------------------------------------------------------
#date           <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d") # this should match the date of ST-GPR modeling
date           <- "2020-09-22"
national_only  <- FALSE  # ignore subnational plots?                              
abc            <- FALSE   # alphabetical order by ihme_loc_id? (otherwise in GBD sort order by super region)
add_old_vers   <- "best" # 
old_vers_round <- 2019   # compare add_old_vers date from this GBD round

# set options 
name <- ""
if (national_only)                                   name <- paste(name, "national", sep="_") else name <- paste(name, "with_subnational", sep="_")
if (!is.null(add_old_vers) & add_old_vers != "NULL") name <- paste(name, "vs", paste0("gbd", old_vers_round), add_old_vers, sep="_")
mean_outpath <- paste0("FILEPATH/mean_hemoglobin", name, "_", date, ".pdf")
total_outpath <- paste0("FILEPATH/total_anemia", name, "_", date, ".pdf")
modsev_outpath <- paste0("FILEPATH/modsev_anemia", name, "_", date, ".pdf")
sev_outpath <- paste0("FILEPATH/sev_anemia", name, "_", date, ".pdf")


# get time-series plots
plot_gpr(date=date, 
         output.mean=mean_outpath,
         output.total=total_outpath,
         output.modsev=modsev_outpath,
         output.sev=sev_outpath,
         add.regions=FALSE, 
         y.axis=FALSE, 
         compare=add_old_vers, 
         compare_round=old_vers_round,
         add.outliers=TRUE)
#***********************************************************************************************************************