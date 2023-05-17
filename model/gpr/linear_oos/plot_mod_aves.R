## script for plotting model averaging results

rm(list=ls())
os <- .Platform$OS.type
if (os=="windows") {
  j<- "FILEPATH"
  h<-"FILEPATH"
} else {
  j<- "FILEPATH"
}

print(.libPaths())
require(data.table )
require(ggplot2 )

######################################
################ ARGS ################
######################################
args <- commandArgs(trailingOnly = TRUE)
param_map_filepath<-args[1]
sexchar<-args[2]
path<-args[3]
age_trend<-as.logical(args[4])

print(param_map_filepath)
print(sexchar)
print(path)
print(age_trend)

# Retrieving array task_id and subsetting to location
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
param_map <- fread(param_map_filepath)
loc <- param_map[task_id,location_id]


##########################################
################ GET DATA ################
##########################################

sqr.s<-readRDS(paste0(path, sexchar, "_data.rds"))
sqr.s<-sqr.s[location_id==loc]

######################################
################ PLOT ################
######################################
n_mods<-sum(grepl("pred", names(sqr.s)))

xvar<-ifelse(age_trend==T, "age_group_id", "year_id")
facet_var<-ifelse(age_trend==T, "year_id", "age_group_name")

## subset to estimation years if plotting by age
if(age_trend==T){
  sqr.s<-sqr.s[year_id %in% c(seq(from=1980, to=2015, by=5), 2017, 2019:2022)]
}

p<-ggplot(data=sqr.s, aes(x=get(xvar)))

for(i in c(2:n_mods, 1)){ 
  if(i==1){
    p<-p+
      geom_line(aes_string(y=paste0("pred", i)), color="green", size=1.5, alpha=.5)
  }else{
    p<-p+
      geom_line(aes_string(y=paste0("pred", i)), color="red", alpha=.3)
  }
}
p<-p+
  geom_line(aes(y=ave_result), color="cornflowerblue", size=1.5, alpha=0.7)+
  facet_wrap(~get(facet_var), scales="free_y")+
  xlab("Year")+
  ylab("Mean")+
  ggtitle(paste0("Submodels and average for ", unique(sqr.s$location_name), ", ", sexchar))+
  theme_bw()+
  theme(text=element_text(size=16))

pdf(file=paste0(path, "subplot_", sexchar, "_", loc,".pdf"), width=11)
print(p)
dev.off()
