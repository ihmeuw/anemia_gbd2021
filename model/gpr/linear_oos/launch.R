###########################################################################################################
### Project: ST-GPR
### Purpose: Launch custom first stage prior testing and assemble for upload to ST-GPR
### Overview: This script is for launching two functions: test_prior() and assemble_prior(). 
###           Together they allow the creation of an ensemble linear prior for STGPR
###########################################################################################################

launch_prior <- function(me, release_id=10, crosswalk_version_id=NA, path_to_data=NA, test_mods=TRUE, average=TRUE, pred_ages,
                         n_mods=50, plot_aves=TRUE, plot_betas=TRUE, age_trend=FALSE,
                         cov_ids, prior_sign, ban_pairs=NULL, polynoms=NULL, modtype="lmer", p_value=0.05,
                         rank_method = "oos.rmse", forms_per_job=30, drop_nids=FALSE, fixed_covs=NULL,
                         custom_covs=NULL, random_effects=NULL, username=username, by_sex, years=c(1980:2022), decomp_step){
  
  #####################################
  ############### PATHS ###############
  #####################################
  output_dir <- paste0("FILEPATH/")
  message(paste0("Creating output directory at: ",output_dir))
  dir.create(output_dir, recursive=TRUE)
  
  rmse_output <- paste0("FILEPATH/", me, "_prior_test_yes_prior_sign", date, ".csv")  # where the object returned by test_prior is saved
  data_output <- paste0("FILEPATH/", me,"_best_data.csv")
  ensemble_output <- paste0("FILEPATH/", me,"_custom_prior_", date, ".csv")
  plot_mods_path <- paste0("/FILEPATH/", me, "/") 
  
  #######################################
  ############### SCRIPTS ###############
  #######################################
  source("FILEPATH/get_ids.R")
  source("FILEPATH/test_prior.R")
  source("FILEPATH/assemble_prior.R")
  source("FILEPATH/bind_covariates.R")
  source("FILEPATH/helpers.R")
  
  #######################################
  ############## PULL COVS ##############
  #######################################
  covs <- get_ids("covariate")
  cov_list <- lapply(cov_ids, function(X) covs[covariate_id==X,covariate_name]) %>% unlist
  
  #######################################
  ############ LAUNCH MODELS ############
  #######################################
  
  if(test_mods==T){
    message(paste0("Testing submodels for me ", me))
    rmses_and_data <- test_prior(crosswalk_version_id=crosswalk_version_id, path_to_data=path_to_data, release_id=release_id, cov_list=cov_list, 
                                 data_transform=data_transform, rank_method=rank_method, modtype=modtype,
                                 custom_covs=custom_covs, fixed_covs=fixed_covs, random_effects=random_effects, ban_pairs=ban_pairs, 
                                 prior_sign = prior_sign, by_sex=by_sex, polynoms=polynoms, ko_prop=ko_prop, kos=kos, drop_nids=FALSE, 
                                 remove_subnats=T, proj=proj, m_mem_free=m_mem_free, username=username, forms_per_job=forms_per_job, years=years,
                                 p_value=p_value)
    
    rmses <- rmses_and_data[[1]]
    data <- rmses_and_data[[2]]
    
    # write out data and rmses
    write.csv(data, file=data_output, row.names=F)
    write.csv(rmses, file=rmse_output, row.names=F)
  }
  
  #####################################################
  ############ CREATE ENSEMBLE PREDICTIONS ############
  #####################################################
  
  if(average==T){
    message("Averaging submodels and predicting")
    
    rmses <- get_recent(output_dir)  
    data <- fread(data_output)
    
    ave_models <- assemble_prior(data, rmses[drop==0,], cov_list, data_transform, pred_ages=pred_ages,
                                 custom_cov_list=custom_covs, polynoms=polynoms, n_mods=n_mods,
                                 plot_mods=plot_aves, age_trend=age_trend, plot_mods_path=plot_mods_path, username=username, proj, 
                                 weight_col=ifelse(rank_method=="out.rmse","out_rmse","aic"), by_sex=by_sex, 
                                 location_set_id=22, release_id=release_id, years=years)
    
    setnames(ave_models, "ave_result", "cv_custom_stage_1")
    
    # drop some uneccessary cols for saving
    ave_models[, c("location_name", "region_name", "super_region_name", "age_group_name"):=NULL]
    
    # Save!
    write.csv(ave_models, file=ensemble_output, row.names=F)
    message(paste0("Success! Ensemble prior saved here: ", ensemble_output))
  }
  
}