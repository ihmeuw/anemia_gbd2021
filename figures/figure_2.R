### Anemia Manuscript Figure 2

rm(list=ls())

## Libraries and functions
library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/map_national_and_subnational.R")

## Metadata
locs <- get_location_metadata(35, gbd_round_id=7)
subnats <- c("BRA","ETH","IDN","ITA","IRN","JPN","KEN","MEX","PAK","PHL","ZAF","SWE","GBR","USA","IND")


#################
### FIGURE 2A ###
#################

## Input data
data_dir <- "FILEPATH"
loc_ids <- paste0(locs$location_id,".csv")
setwd(data_dir)
data_2a <- lapply(loc_ids,fread) %>% rbindlist()

## Format
data_2a <- data_2a[year_id==2021 & age_group_id==22 & measure=="prevalence" & severity=="total"]
data_2a[,mean:=mean*100]
data_2a <- merge(data_2a, locs[,.(location_id,ihme_loc_id)], by="location_id", all.x=TRUE)

## Plot
breaks <- c(4.4, 6.3, 7.5, 9.2, 13.2, 16.2, 18.6, 21.0, 27.8, 42.8, 67.3)
plot2a <- map_national_and_subnational(data=data_2a, map.var="mean", subnat.style="in.map", subnat.level="custom",
                                       scale="cat", breaks=breaks, labels=NULL, unit.type="count", custom.subnats=subnats,
                                       legend.title="Prevalence of anaemia (%)", col.rev=TRUE, legend.title.size=8)



#################
### FIGURE 2B ###
#################

## Input data
data_dir <- "FILEPATH"
loc_ids <- paste0(locs$location_id,".csv")
setwd(data_dir)
data_2b <- lapply(loc_ids,fread) %>% rbindlist()

## Format
data_2b <- data_2b[year_id==2021 & age_group_id==22 & measure=="yld_rate" & severity=="total"]
data_2b[,mean:=mean*100000]
data_2b <- merge(data_2b, locs[,.(location_id,ihme_loc_id)], by="location_id", all.x=TRUE)

## Plot
breaks <- c(99.5, 160.7, 205.9, 240.3, 329.3, 403.2, 518.6, 637.8, 800.8, 1298.2, 2856.6)
plot2b <- map_national_and_subnational(data=data_2b, map.var="mean", subnat.style="in.map", subnat.level="custom",
                                       scale="cat", breaks=breaks, labels=NULL, unit.type="count", custom.subnats=subnats,
                                       legend.title="Anaemia YLDs\n(rate per 100,000 population)", 
                                       col.rev=TRUE, legend.title.size=8)



#################
### FIGURE 2C ###
#################

## Input data - EPI TRANSITION ANALYSIS
data_dir <- "FILEPATH"
data_2c <- fread(paste0(data_dir,"/summary_results.csv"))

map_dt <- data_2c[measure_id == 3 & age_group_name == "All Ages" & year_id == 2021]

breaks <- c(0.20, 0.50, 0.75, 0.90, 1.0, 1.5, 2.5, 3.5, 5.5)
plot2c <- map_national_and_subnational(data=map_dt, map.var="ratio", subnat.style="in.map", subnat.level="custom",
                                       scale="cat", breaks=breaks, labels=NULL, unit.type="count", custom.subnats=subnats,
                                       legend.title="Observed/Expected\nYLD rate", 
                                       col.rev=TRUE, legend.title.size=8)



#################
### FIGURE 2D ###
#################

## Input data
data_dir <- "FILEPATH"
loc_ids <- paste0(locs$location_id,".csv")
setwd(data_dir)
data_2d <- lapply(loc_ids,fread) %>% rbindlist()

## Format
data_2d <- data_2d[year_id==2021 & age_group_id==22 & measure=="yld_rate" & severity=="total"]
data_2d <- data_2d[,.(location_id, mean, sex_id)]
data_2d <- dcast(data_2d, location_id ~ sex_id, value.var="mean")
data_2d[, mean := `2`/`1`]

data_2d <- merge(data_2d, locs[,.(location_id,ihme_loc_id)], by="location_id", all.x=TRUE)

## Plot
breaks <- c(0.45, 1.00, 1.65, 1.95, 2.15, 2.30, 2.47, 2.75, 3.07, 3.36, 7.25)
colors <- c("gray",rev(RColorBrewer::brewer.pal(9,"RdYlBu")))
plot2d <- map_national_and_subnational(data=data_2d, map.var="mean", subnat.style="in.map", subnat.level="custom",
                                       scale="cat", breaks=breaks, labels=NULL, unit.type="count", custom.subnats=subnats,
                                       legend.title="Anaemia YLD rate\nfemale/male ratio", legend.title.size=8,
                                       custom.categorical.colors=colors)


# Combine
fig2a <- ggarrange(plot2a, plot2b, ncol=2, labels=c("A","B"))
fig2b <- ggarrange(plot2c, plot2d, ncol=2, labels=c("C","D"))
fig2 <- ggarrange(fig2a, fig2b, nrow=2)
fig2_final <- annotate_figure(fig2, fig.lab="Figure 2 All-ages anaemia burden, 2021", fig.lab.pos="bottom.left", fig.lab.face="bold", fig.lab.size=14)
