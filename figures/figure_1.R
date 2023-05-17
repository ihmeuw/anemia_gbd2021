### Anemia Manuscript Figure 1

rm(list=ls())

## Libraries and functions
library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_pct_change.R")
source("FILEPATH/get_outputs.R")


## Metadata
locs <- get_location_metadata(35, gbd_round_id=7)


#################
### FIGURE 1A ###
#################

## Input data
data_dir <- "FILEPATH/"
data_1a <- fread(paste0(data_dir,"/1.csv"))

## Format
data_1a <- data_1a[measure=="prevalence" & severity!="total"]
data_1a[,mean:=mean*100]
data_1a[,severity:=ifelse(severity=="mild","Mild",
                          ifelse(severity=="moderate","Moderate","Severe"))]

## Plot
colors <- c("#1B9E77", "#D95F02", "#7570B3")

plot1a <- ggplot(data=data_1a, aes(x=year_id, y=mean, fill=severity)) + 
                geom_area() +
                theme_classic() + 
                scale_x_continuous(limits = c(1990,2021), breaks=seq(1990,2020,5),expand = c(0, 0)) +
                scale_y_continuous(limits = c(0,30), breaks=seq(0,30,5), expand = c(0, 0)) +
                labs(title="Global anaemia prevalence by severity, 1990-2021",
                     y="Anaemia prevalence (%)", x="Year", fill="Severity") +
                scale_fill_manual(values=colors) +
                theme(text=element_text(size=12),
                      axis.text=element_text(size=14),
                      axis.title=element_text(size=16),
                      plot.title=element_text(size=20),
                      legend.title=element_text(size=16, color="white"),
                      legend.text=element_text(size=14, color="white"),
                      axis.text.x = element_text(angle = 30,vjust=1,hjust=1))+
                      guides(fill = guide_legend(override.aes = list(fill=NA)))
  


#################
### FIGURE 1B ###
#################

## Input data
data_dir <- "FILEPATH"
data_1b <- fread(paste0(data_dir,"/1.csv"))

## Format
data_1b <- data_1b[measure=="yld_rate" & severity!="total"]
data_1b[,mean:=mean*100000]
data_1b[,severity:=ifelse(severity=="mild","Mild",
                          ifelse(severity=="moderate","Moderate","Severe"))]

## Plot
colors <- c("#1B9E77", "#D95F02", "#7570B3")

plot1b <- ggplot(data=data_1b, aes(x=year_id, y=mean, fill=severity)) + 
            geom_area() +
            theme_classic() + 
            scale_x_continuous(limits = c(1990,2021), breaks=seq(1990,2020,5), expand = c(0, 0)) +
            scale_y_continuous(limits = c(0,1000), expand = c(0, 0)) +
            labs(title="Global anaemia YLD rate by severity, 1990-2021",
                 y="Anaemia YLDs (rate per 100,000 population)", x="Year", fill="Severity") +
            scale_fill_manual(values=colors) +
            theme(text=element_text(size=12),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  plot.title=element_text(size=20),
                  legend.title=element_text(size=16),
                  legend.text=element_text(size=14),
                  axis.text.x = element_text(angle = 30,vjust=1,hjust=1))


#################
### FIGURE 1C ###
#################

## Input data
age_ids <- c(2,3,388,389,238,34,6:20,30:32,235,22)
age_map <- data.table(order=1:26,
                      age_group_id=age_ids,
                      age_group_name=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                       "12-23 months", "2 to 4","5 to 9","10 to 14","15 to 19","20 to 24",
                                       "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54",
                                       "55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84",
                                       "85 to 89","90 to 94","95 plus","All Ages"))

data_1c <- get_outputs("rei", rei_id=c(192), measure_id=c(3,5), location_id=1, year_id=2021, 
                       age_group_id=age_ids, sex_id=c(1,2), cause_id=294, metric_id=3, 
                       gbd_round_id=7, decomp_step="iterative", compare_version_id=7846)[,.(age_group_id, sex_id, measure_id, rei_id, val, lower, upper)]

data_1c <- merge(data_1c, age_map, by="age_group_id",all.x=TRUE)
data_1c[, `:=` (sex = ifelse(sex_id==1,"Males","Females"))]
data_1c[, age_group_name:=factor(age_group_name, levels=unique(age_map$age_group_name))]
data_1c[, sex:=factor(sex, levels=c("Males","Females"))]
data_1c[, measure:=ifelse(measure_id==5, "Prevalence (%)","YLDs (Rate per 100,000)")]
data_1c[measure_id==3, `:=` (val = val*100000,
                             lower = lower*100000,
                             upper = upper*100000)]
data_1c[measure_id==5, `:=` (val = val*100,
                             lower = lower*100,
                             upper = upper*100)]

plot1c <- ggplot(data=data_1c,aes(x=age_group_name,y=val,ymin=lower,ymax=upper,fill=sex)) + facet_wrap(~measure,nrow=2,scales="free_y") +
            geom_bar(stat="identity",position = position_dodge()) + 
            geom_linerange(position=position_dodge(width=0.9)) +
            theme_classic() + 
            labs(title="Global anaemia prevalence and YLDs by age and sex, 2021",
                 y="", x="", fill="Sex") +
            scale_fill_manual(values=c('#8c181e','#cca72c')) + 
            theme(text=element_text(size=12),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  plot.title=element_text(size=20),
                  legend.title=element_text(size=14, color="white"),
                  legend.text=element_text(size=14, color="white"),
                  axis.text.x = element_text(angle = 45,vjust=1,hjust=1))+
            guides(fill = guide_legend(override.aes = list(fill=NA)))


#################
### FIGURE 1D ###
#################

## Input data
age_ids <- c(2,3,388,389,238,34,6:20,30:32,235,22)
age_map <- data.table(order=1:26,
                      age_group_id=age_ids,
                      age_group_name=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                       "12-23 months", "2 to 4","5 to 9","10 to 14","15 to 19","20 to 24",
                                       "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54",
                                       "55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84",
                                       "85 to 89","90 to 94","95 plus","All Ages"))

data_1d <- get_pct_change("rei_id", 192, year_start_id=1990, year_end_id=2021, source="como", change_type="pct_change_rate",
                          measure_id=c(3,5), location_id=1, sex_id=c(1,2), age_group_id=age_ids,
                          gbd_round_id=7, decomp_step="iterative", version_id=1353)[cause_id==294, .(location_id,sex_id,age_group_id,measure_id,mean,lower,upper)]

data_1d <- merge(data_1d, age_map, by="age_group_id",all.x=TRUE)
data_1d[, `:=` (sex = ifelse(sex_id==1,"Males","Females"),
                measure=ifelse(measure_id==3,"YLDs (rate per population)","Prevalence (%)"))]
data_1d[, age_group_name:=factor(age_group_name, levels=unique(age_map$age_group_name))]
data_1d[, sex:=factor(sex, levels=c("Males","Females"))]

plot1d <- ggplot(data=data_1d,aes(x=age_group_name,y=mean*100,ymin=lower*100,ymax=upper*100,fill=sex)) + facet_wrap(~measure,nrow=2) +
                  geom_bar(stat="identity",position = position_dodge()) + 
                  geom_linerange(position=position_dodge(width=0.9)) + 
                  theme_classic() + 
                  scale_fill_manual(values=c('#8c181e','#cca72c')) + 
                  labs(x="",y="Percent change (1990-2020)",
                       title="Percent change in anaemia prevalence and YLDs, 1990-2021") + 
                  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1),
                        legend.title = element_blank(),
                        text=element_text(size=12),
                        axis.text=element_text(size=14),
                        axis.title=element_text(size=16),
                        plot.title=element_text(size=20),
                        legend.text=element_text(size=14),
                        strip.text=element_text(size=14))


# Combine
fig1a <- ggarrange(plot1a, plot1b, ncol=2, labels=c("A","B"),common.legend=FALSE)
fig1b <- ggarrange(plot1c, plot1d, ncol=2, labels=c("C","D"),common.legend=FALSE)
fig1 <- ggarrange(fig1a, fig1b, nrow=2)
fig1_final <- annotate_figure(fig1, fig.lab="Figure 1 Anaemia prevalence and YLDs (rate per 100,000 population)", fig.lab.pos="bottom.left", fig.lab.face="bold", fig.lab.size=14)