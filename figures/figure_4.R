### Anemia Manuscript Figure 4

rm(list=ls())

## Libraries and functions
library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(openxlsx)
library(ggpubr)
source("FILEPATH/get_ids.R")
source("FILEPATH/get_model_results.R")
source("FILEPATH/get_location_metadata.R")

## Metadata
locs <- get_location_metadata(35, gbd_round_id=7)

age_ids <- c(2,3,388,389,238,34,6:20,30:32,235,22,27)
age_map <- data.table(order=1:27,
                      age_group_id=age_ids,
                      age_group_name=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                       "12 to 23 months", "2 to 4","5 to 9","10 to 14","15 to 19","20 to 24",
                                       "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54",
                                       "55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84",
                                       "85 to 89","90 to 94","95 plus","All Ages","Age-standardised"))

inmap <- read.xlsx('FILEPATH/in_out_meid_map_2020.xlsx') %>% data.table
outmap <- read.xlsx('FILEPATH/in_out_meid_map_2020.xlsx', sheet = 'out_meids') %>% data.table
allmap <- merge(inmap, outmap, by='subtype') %>%
  arrange(order)
cause_name_map <- fread("FILEPATH/cause_name_map.csv")
input_ref <- fread("FILEPATH/anemia_ca_mvdf_10109_147_input.csv")

#################
### FIGURE 5A ###
#################

## Input data
data_dir <- "FILEPATH"
data_5a <- fread(paste0(data_dir,"/1.csv"))

## Format
data_5a[,c("moderate_prevalence","severe_prevalence","mild_prevalence"):=NULL]
data_5a[,total_prevalence := total_prevalence*100]


endo_other <- data_5a[cause_name=="Endocrine - Other"]
data_5a <- rbind(data_5a[!cause_name=="Endocrine - Other"], endo_other)
data_5a$cause_name <- factor(data_5a$cause_name, levels = unique(data_5a$cause_name))
data_5a <- merge(data_5a, age_map, by = "age_group_id", all.x=TRUE)
data_5a[, age_group_name:=factor(age_group_name,levels=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                                    "12 to 23 months","2 to 4","5 to 9","10 to 14","15 to 19",
                                                    "20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                                    "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69",
                                                    "70 to 74","75 to 79","80 to 84","85 to 89","90 to 94",
                                                    "95 plus","All Ages","Age-standardised"))]
data_5a <- merge(data_5a,cause_name_map,by="cause_name",all.x=TRUE)
data_5a[, cause_name_short := factor(cause_name_short, levels=unique(cause_name_map$cause_name_short))]

## Plot
customColors <- c("#0006FD", "#0072B2", "#0080FF", "#56B4E9", "#004867", "#42F4F4", "#21B2B2", "#0B6666", 
                  "#FF00F9", "#800080", "#DB0026", 
                  "#AFA06D", "#EBDB9E", "#D4CD8A", "#EDE587", "#E3C25D", "#4C381E", "#A38446", "#997423", "#DBCF21", "#FFF644", "#FFFF25", "#F3F707", 
                  "#00B22B", "#009E73",   
                  "#E69F00", "#F77500", "#D55E00", "#FF0000", "#F99363", 
                  "#4F4F4C", "#70706D", "#000000", "#DBDAD0",
                  "#330066", "#9933CC", "#CC66FF")

plot5a <- ggplot(data_5a, aes(x=age_group_name, y = total_prevalence, fill = cause_name_short)) +
                geom_bar(stat="identity") + theme_classic() + 
                labs(fill="Cause", y="Prevalence (%)",x="Age",
                     title="Cause-Specific Anaemia Prevalence - 2021, Both Sexes") + 
                scale_fill_manual(NULL, values=customColors, drop=F, guide=guide_legend(ncol=1)) +
                scale_y_continuous(expand = c(0, 0)) +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size = 14, angle=45, hjust=1, vjust=1, color="black"),
                      axis.text.y = element_text(size=14, color="black"),
                      plot.title = element_text(size=16),
                      legend.background = element_blank(),
                      legend.box.background = element_rect(colour = "black"),
                      axis.title = element_text(size=16),
                      plot.margin = margin(5.5,5.5,5.5,16, "pt")) +
                guides(fill=guide_legend(keyheight=1, ncol=1)) 



#################
### FIGURE 5B ###
#################

## Input data
data_dir <- "FILEPATH" 
data_5b <- fread(paste0(data_dir,"/1.csv"))

## Format
data_5b[,c("moderate_yld","severe_yld","mild_yld"):=NULL]
data_5b <- data_5b[!sex_id==3]
data_5b <- dcast(data_5b, location_id + sex_id + age_group_id + cause_name ~ year_id, value.var="total_yld")
data_5b[, pct_change_yld := ((`2021` - `1990`)/`1990`) * 100]

endo_other <- data_5b[cause_name=="Endocrine - Other"]
data_5b <- rbind(data_5b[!cause_name=="Endocrine - Other"], endo_other)
data_5b$cause_name <- factor(data_5b$cause_name, levels = unique(data_5b$cause_name))
data_5b <- merge(data_5b, age_map, by = "age_group_id", all.x=TRUE)
data_5b[, age_group_name:=factor(age_group_name,levels=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                                         "12 to 23 months","2 to 4","5 to 9","10 to 14","15 to 19",
                                                         "20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                                         "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69",
                                                         "70 to 74","75 to 79","80 to 84","85 to 89","90 to 94",
                                                         "95 plus","All Ages","Age-standardised"))]
data_5b <- merge(data_5b,cause_name_map,by="cause_name",all.x=TRUE)
data_5b[, cause_name_short := factor(cause_name_short, levels=unique(cause_name_map$cause_name_short))]

## Pull cause-specific prevalence

meid_measures <- inmap[!is.na(modelable_entity_id), ][, .(modelable_entity_id, measure_id)]

subtypes <- rbindlist(lapply(meid_measures$modelable_entity_id, function(me) {
  print(sprintf("reading subtype data for modelable_entity_id %s", me))
  subtype <- get_model_results("epi", gbd_id=me, 
                               measure_id=meid_measures[modelable_entity_id==me, ]$measure_id,
                               age_group_id=22, location_id=1, sex_id=c(1,2), year_id=c(1990,2021),
                               gbd_round_id=7, decomp_step="iterative",
                               model_version_id=input_ref[modelable_entity_id==me, model_version_id])[,.(location_id,year_id,age_group_id,sex_id,mean)]
    
 
  subtype[, subtype := as.character(inmap[modelable_entity_id==me, ]$subtype)]
  return(subtype)
}), use.names=TRUE)

subtypes[subtype %like% "gyne" & sex_id==1, mean:=0]

subtypes <- dcast(subtypes, location_id + age_group_id + sex_id + subtype ~ year_id, value.var="mean")
subtypes[, pct_change_cause := ((`2021` - `1990`)/`1990`)*100]
subtypes[, c("1990","2021"):=NULL]

subtypes <- merge(subtypes, outmap[,.(subtype,hierarchy_4)], by="subtype", all.x=TRUE)
subtypes <- merge(subtypes, cause_name_map, by.x="hierarchy_4", by.y="cause_name", all.x=TRUE)

subtypes <- subtypes[,.(cause_name_short,sex_id,pct_change_cause)]

# merge
data_5b <- merge(data_5b, subtypes, by=c("cause_name_short","sex_id"), all.x=TRUE)
data_5b[, sex := ifelse(sex_id==1,"Males","Females")]
data_5b[, sex := factor(sex, levels=c("Males","Females"))]
data_5b[, cause_name_short := factor(cause_name_short, levels=unique(cause_name_map$cause_name_short))]

## Plot
customColors <- c("#0006FD", "#0072B2", "#0080FF", "#56B4E9", "#004867", "#42F4F4", "#21B2B2", "#0B6666", 
                  "#FF00F9", "#800080", "#DB0026", 
                  "#AFA06D", "#EBDB9E", "#D4CD8A", "#EDE587", "#E3C25D", "#4C381E", "#A38446", "#997423", "#DBCF21", "#FFF644", "#FFFF25", "#F3F707", 
                  "#00B22B", "#009E73",   
                  "#E69F00", "#F77500", "#D55E00", "#FF0000", "#F99363", 
                  "#4F4F4C", "#70706D", "#000000", "#DBDAD0",
                  "#330066", "#9933CC", "#CC66FF")

plot5b <- ggplot(data_5b, aes(x=pct_change_cause, y = pct_change_yld, color = cause_name_short)) +
  geom_vline(xintercept=0, color="gray", alpha=0.7) +
  geom_hline(yintercept=0, color="gray", alpha=0.7) +
  geom_abline(intercept=0, slope=1, color="black") +
  geom_point(size=2.5, alpha=0.6) + 
  facet_wrap(~sex, ncol=2) + 
  theme_classic() + 
  scale_x_continuous(limits = c(-115,325), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-115,225), expand = c(0, 0)) +
  labs(y="Percent change in cause-specific \nanaemia YLD rate, 1990-2021",
       x="Percent change in cause prevalence, 1990-2021",
       title="Change in anaemia YLD rate vs. cause prevalence, 1990-2021") + 
  scale_color_manual(NULL, values=customColors) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14, angle=45, hjust=1, vjust=1, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        plot.title = element_text(size=16),
        axis.title = element_text(size=16))



#################
### FIGURE 5C ###
#################

## Input data
data_dir <- "FILEPATH"
data_5c <- fread(paste0(data_dir,"/1.csv"))

## Format
data_5c[,c("moderate_yld","severe_yld","mild_yld"):=NULL]
data_5c[,total_yld := total_yld*100000]


endo_other <- data_5c[cause_name=="Endocrine - Other"]
data_5c <- rbind(data_5c[!cause_name=="Endocrine - Other"], endo_other)
data_5c$cause_name <- factor(data_5c$cause_name, levels = unique(data_5c$cause_name))
data_5c <- merge(data_5c, age_map, by = "age_group_id", all.x=TRUE)
data_5c[, age_group_name:=factor(age_group_name,levels=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                                         "12 to 23 months","2 to 4","5 to 9","10 to 14","15 to 19",
                                                         "20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                                         "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69",
                                                         "70 to 74","75 to 79","80 to 84","85 to 89","90 to 94",
                                                         "95 plus","All Ages","Age-standardised"))]
data_5c <- merge(data_5c,cause_name_map,by="cause_name",all.x=TRUE)
data_5c[, cause_name_short := factor(cause_name_short, levels=unique(cause_name_map$cause_name_short))]

## Plot
customColors <- c("#0006FD", "#0072B2", "#0080FF", "#56B4E9", "#004867", "#42F4F4", "#21B2B2", "#0B6666", 
                  "#FF00F9", "#800080", "#DB0026", 
                  "#AFA06D", "#EBDB9E", "#D4CD8A", "#EDE587", "#E3C25D", "#4C381E", "#A38446", "#997423", "#DBCF21", "#FFF644", "#FFFF25", "#F3F707", 
                  "#00B22B", "#009E73",   
                  "#E69F00", "#F77500", "#D55E00", "#FF0000", "#F99363", 
                  "#4F4F4C", "#70706D", "#000000", "#DBDAD0",
                  "#330066", "#9933CC", "#CC66FF")

plot5c <- ggplot(data_5c, aes(x=age_group_name, y = total_yld, fill = cause_name_short)) +
  geom_bar(stat="identity") + theme_classic() + 
  labs(fill="Cause", y="YLDs (rate per 100,000 population)",x="Age",
       title="Cause-Specific Anaemia YLD rate - 2021, Males") + 
  scale_fill_manual(NULL, values=customColors, drop=F, guide=guide_legend(ncol=1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle=45, hjust=1, vjust=1, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        plot.title = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.title = element_text(size=16)) +
  guides(fill=guide_legend(keyheight=1, ncol=1))



#################
### FIGURE 5D ###
#################

## Input data
data_dir <- "FILEPATH"
data_5d <- fread(paste0(data_dir,"/1.csv"))

## Format
data_5d[,c("moderate_yld","severe_yld","mild_yld"):=NULL]
data_5d[,total_yld := total_yld*100000]


endo_other <- data_5d[cause_name=="Endocrine - Other"]
data_5d <- rbind(data_5d[!cause_name=="Endocrine - Other"], endo_other)
data_5d$cause_name <- factor(data_5d$cause_name, levels = unique(data_5d$cause_name))
data_5d <- merge(data_5d, age_map, by = "age_group_id", all.x=TRUE)
data_5d[, age_group_name:=factor(age_group_name,levels=c("Early Neonatal","Late Neonatal","1-5 months","6-11 months",
                                                         "12 to 23 months","2 to 4","5 to 9","10 to 14","15 to 19",
                                                         "20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                                         "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69",
                                                         "70 to 74","75 to 79","80 to 84","85 to 89","90 to 94",
                                                         "95 plus","All Ages","Age-standardised"))]
data_5d <- merge(data_5d,cause_name_map,by="cause_name",all.x=TRUE)
data_5d[, cause_name_short := factor(cause_name_short, levels=unique(cause_name_map$cause_name_short))]

## Plot
customColors <- c("#0006FD", "#0072B2", "#0080FF", "#56B4E9", "#004867", "#42F4F4", "#21B2B2", "#0B6666", 
                  "#FF00F9", "#800080", "#DB0026", 
                  "#AFA06D", "#EBDB9E", "#D4CD8A", "#EDE587", "#E3C25D", "#4C381E", "#A38446", "#997423", "#DBCF21", "#FFF644", "#FFFF25", "#F3F707", 
                  "#00B22B", "#009E73",   
                  "#E69F00", "#F77500", "#D55E00", "#FF0000", "#F99363", 
                  "#4F4F4C", "#70706D", "#000000", "#DBDAD0",
                  "#330066", "#9933CC", "#CC66FF")

plot5d <-ggplot(data_5d, aes(x=age_group_name, y = total_yld, fill = cause_name_short)) +
  geom_bar(stat="identity") + theme_classic() + 
  labs(fill="Cause", y="YLDs (rate per 100,000 population)",x="Age",
       title="Cause-Specific Anaemia YLD rate - 2021, Females") + 
  scale_fill_manual(NULL, values=customColors, drop=F, guide=guide_legend(ncol=1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle=45, hjust=1, vjust=1, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        plot.title = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size=16),
        axis.title = element_text(size=16)) +
  guides(fill=guide_legend(keyheight=1.45, ncol=1))

leg <- get_legend(plot5d)


# Combine
fig5 <- ggarrange(plot5a, plot5b, plot5c, plot5d, ncol=2, nrow=2, legend="right", labels=c("A","B","C","D"), legend.grob = leg)
fig5_final <- annotate_figure(fig5, fig.lab="Figure 4 Global distribution of anaemia causes", fig.lab.pos="bottom.left", fig.lab.face="bold", fig.lab.size=18)
