## Upload data to anemia bundles

# Define Functions
source("FILEPATH/custom_db.R")
source("FILEPATH/get_ids.R")
mes <- get_ids("modelable_entity")
locs <- fread("FILEPATH/locs.csv")

prep_data <- function(data_new, me_id){
  data_new[, underlying_nid := NA]
  data_new[, sex := ifelse(sex_id==1,"Male",ifelse(sex_id==2,"Female","Both"))]
  data_new[, year_id := year_start]
  data_new[, year_end := year_start]
  data_new[, age_group_id := 999]
  data_new[, orig_age_start := age_start]
  data_new[, orig_age_end := age_end]
  data_new[, age_start := NULL]
  data_new[, age_end := NULL]
  setnames(data_new,"mean","val")
  data_new[, variance := standard_error^2]
  data_new[, seq := ""]
  data_new[, is_outlier := 0]
  data_new[variance==0, variance:=0.01]
  data_new[, modelable_entity_id := me_id]
  data_new[, modelable_entity_name := mes[modelable_entity_id==me_id,modelable_entity_name]]
  if (me_id %in% c(10487,26138)){
    data_new[, measure := "continuous"]
  } else{
    data_new[, measure := "proportion"]
  }
  
  return(data_new)
}

upload_bundle <- function(df, bundle, decomp_step){
  df[, bundle_id := bundle]
  FOLDER <- "imp_anemia"
  BUNDLE <- bundle
  upload_data(BUNDLE, FOLDER, df, decomp_step)
}

# Read in Data
df <- fread("/FILEPATH/collapse_anemia_2020-08-26.csv")
df <- df[!ihme_loc_id==""]
df <- merge(df,locs[,.(ihme_loc_id,location_id)],by="ihme_loc_id",all.x=TRUE)

## Non-Pregnant
nonpreg <- df[cv_pregnant==0]
hemoglobin <- nonpreg[var=="hemoglobin"]
anemic <- nonpreg[var=="anemia_anemic"]
modsev <- nonpreg[var=="anemia_mod_sev"]
sev <- nonpreg[var=="anemia_severe"]

## Pregnant
preg <- df[cv_pregnant==1]
hemoglobin_p <- preg[var=="hemoglobin"]
anemic_p <- preg[var=="anemia_anemic"]
modsev_p <- preg[var=="anemia_mod_sev"]
sev_p <- preg[var=="anemia_severe"]


# Upload Data 

## Non_pregnant

### Anemia
anemic <- prep_data(anemic,25318)
upload_bundle(anemic, 8108, "iterative")

### Mod-Sev
modsev <- prep_data(modsev,25322)
upload_bundle(modsev, 8120, "iterative")

### Severe
sev <- prep_data(sev,25321)
upload_bundle(sev, 8117, "iterative")

## Pregnant

### Mean Hemoglobin
hemoglobin_p <- prep_data(hemoglobin_p,26138)
hemoglobin_p <- hemoglobin_p[orig_age_start >= 10 & orig_age_end <= 55]
upload_bundle(hemoglobin_p, 8342, "iterative")

### Anemia
anemic_p <- prep_data(anemic_p,25323)
anemic_p <- anemic_p[orig_age_start>=10 & orig_age_end<=55]
anemic_p <- anemic_p[!is.na(variance)]
upload_bundle(anemic_p, 8123, "iterative")

### Mod-Sev
modsev_p <- prep_data(modsev_p,25327)
modsev_p <- modsev_p[orig_age_start>=10 & orig_age_end<=55]
modsev_p <- modsev_p[!is.na(variance)]
upload_bundle(modsev_p, 8135, "iterative")

### Severe
sev_p <- prep_data(sev_p,25326)
sev_p <- sev_p[orig_age_start>=10 & orig_age_end<=55]
sev_p <- sev_p[!is.na(variance)]
upload_bundle(sev_p, 8132, "iterative")

