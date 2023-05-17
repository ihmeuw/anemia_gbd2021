###########################################################################################################
### Date: 06/12/2019
### Project: ST-GPR
### Purpose: Calculate out of sample RMSE for one sex model
###########################################################################################################

########################################
################ SET-UP ################
########################################

rm(list=ls())
os <- .Platform$OS.type

if (os=="windows") {
  j<- "FILEPATH"
  h<-"FILEPATH"
} else {
  username <- Sys.info()[["user"]]
  j<- "FILEPATH"
  h<-paste0("FILEPATH",username)
}

print(.libPaths())
library(data.table )
library(lme4 )
library(MuMIn )
library(boot )
library(ggplot2 )
library(rhdf5 )
library(arm )
library(haven)
library(plyr )
library(dplyr )
library(stringr )
library(DBI )

##############################################
################ ARGS & PATHS ################
##############################################

args <- commandArgs(trailingOnly = TRUE)
start<-as.integer(args[1])
end<-as.integer(args[2])
sexchar<- args[3]
data_path<-args[4]
forms_path<-args[5]
output_folder<-args[6]
date<-args[7]
data_transform<-args[8]
modtype<-args[9]
kos<-as.integer(args[10])

central<-paste0("FILEPATH")
source(paste0(central, "get_location_metadata.R"))

suppressMessages(F)

###########################################
################ PULL DATA ################
###########################################
data<-readRDS(data_path)
forms<-readRDS(forms_path)

for(index in start:end){ 

  output<-paste0(output_folder, "form_", sexchar, "_", index, ".csv")

  suppressMessages({
  message(forms)

  message(paste("class of forms:", class(forms), length(forms)))

  message(paste("i:", index, class(index)))

  message(forms[index])

  message(class(forms[index]))

  form<-forms[index]

  message(paste0("pre-whitespace clean form:", form))
  })
  if(index>length(forms)){ stop("Formula index larger than length of formulas!" )}

  ##########################################
  ############ PRED.LM FUNCTION ############
  ##########################################

  pred.lm <- function(df, model, predict_re=0) {
    ## RE form
    re.form <- ifelse(predict_re==1, NULL, NA)
    ## Predict
    if (class(model) == "lmerMod") {
      prior <- predict(model, newdata=df, allow.new.levels=T, re.form=re.form)
    } else {
      prior <- predict(model, newdata=df)
    }
    return(prior)
  }
  
  #################################################
  ############ TRANSFORM_DATA FUNCTION ############
  #################################################
  
  transform_data <- function(var, space, reverse=F) {
    if (space == "logit" & reverse==F) {
      var <- logit(var)
    } else if (space == "logit" & reverse==T) {
      var <- inv.logit(var)
    } else if (space == "log" & reverse==F) {
      var <- log(var)
    } else if (space == "log" & reverse==T) {
      var <- exp(var)
    }
    
    return(var)
    
  }

  #################################################
  ################ CALCULATE RMSES ################
  #################################################

  message(paste("Formula:", form))

  data[, ko0:=0]

  rmses<-list()
  for(ko in 0:kos){ 
    if(ko==0){
      message(paste(" Calculating in-sample RMSE"))
    }else{
      message(paste(" Calculating out-of-sample RMSE for holdout", ko))
    }

    test<-data[get(paste0("ko", ko))==1, ]
    test<-as.data.table(test)
    train<-data[get(paste0("ko", ko))==0, ]

    if(ko==0){
      test<-data 
      train<-data
    }

    covs<-list()
    message(paste("Calculating RMSE for formula", index))

    if(modtype=="lmer"){
      mod<-lmer(as.formula(form), data=train)
    }

    if(modtype=="lm"){
      mod<-lm(as.formula(form), data=train)
    }
    preds<-pred.lm(test, mod, predict_re=0)

    if(length(preds)!= nrow(test)){
      stop("Prediction not done for each row, there may be missing values in the data or a covariate!")
    }

    test.t<-cbind(test, preds)

    test.t[, preds:=transform_data(preds, data_transform, reverse=T)] 
    test.t[, sqrerr:=(data-preds)^2]
    rmse<-sqrt(mean(test.t$sqrerr, na.rm=T))

    covs<-as.character(as.formula(form))[3]

    if(ko==0){
      is.rmse<-rmse

      if(modtype=="lmer"){
        fixd<-fixef(mod)
        fixd_se<-se.fixef(mod)
      }
      if(modtype=="lm"){
        fixd<-coef(mod)
        fixd_se<-coef(summary(mod))[, 2]
      }
      fixd<-fixd[!grepl("age_group_id", names(fixd))]  

      fixd_se<-fixd_se[!grepl("age_group_id", names(fixd_se))]

      fixd<-data.table(cov=names(fixd), fixd, fixd_se)

      fixd.m<-melt(fixd, measure.vars=c("fixd", "fixd_se"))
      effects<-dcast(fixd.m, .~cov+variable)
      effects[, .:=NULL]

      message("Calculating AIC")
      aic<-AIC(logLik(mod))

    }else{

      rmses[[ko]]<-rmse
    }

  } ##########################END KO LOOP###################

  if(length(rmses)==0){
    rmse<-"No holdouts done"
  }else{
    rmse<-unlist(rmses) %>% mean
  }

  out<-data.table(out_rmse=rmse, in_rmse=is.rmse, aic=aic, covs=covs, sex=sexchar)
  out.t<-cbind(out, effects)

  rm(mod) 

  write.csv(out.t, file=output, row.names=F)

  message(paste0("Output saved to", output))
}
