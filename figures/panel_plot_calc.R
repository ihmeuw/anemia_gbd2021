rm(list=ls())

## Libraries and functions
library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(openxlsx)
library(Cairo)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_pct_change.R")
source("FILEPATH/get_outputs.R")
source("FILEPATH/get_draws.R")
source("FILEPATH/get_ids.R")
source("FILEPATH/collapse_point.R")

## Source args
args <- commandArgs(trailingOnly = TRUE)
param_map_filepath <- args[1] 
print(param_map_filepath)


task_id <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
param_map <- fread(param_map_filepath) 

## Metadata
loc_id <- param_map[task_id, location_id]
age_ids <- c(2,3,388,389,238,34,6:20,30:32,235,22,27)
age_map <- data.table(order=1:27,
                      age_group_id=age_ids,
                      age_group_name=c("Early Neonatal","Late Neonatal","1 to 5 months","6 to 11 months",
                                       "12 to 23 months", "2 to 4","5 to 9","10 to 14","15 to 19","20 to 24",
                                       "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54",
                                       "55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84",
                                       "85 to 89","90 to 94","95 plus","All Ages","Age-standardised"))
loc_meta <- get_location_metadata(35,gbd_round_id=7)


## Time Series of Anemia Prevalence by Sex
total <- get_outputs("rei", rei_id=192, measure_id=5, location_id=loc_id, year_id=c(1990:2021),
                     age_group_id=age_ids, sex_id=3, metric_id=3, gbd_round_id=7, decomp_step="iterative",
                     compare_version_id=7846, cause_id=294)[!is.na(val),.(location_id, location_name, year_id, sex_id, sex, age_group_id, age_group_name, val, lower, upper)]
total[,severity:="Total"]

severe <- get_outputs("rei", rei_id=207, measure_id=5, location_id=loc_id, year_id=c(1990:2021),
                      age_group_id=age_ids, sex_id=3, metric_id=3, gbd_round_id=7, decomp_step="iterative",
                      compare_version_id=7846, cause_id=294)[!is.na(val),.(location_id, location_name, year_id, sex_id, sex, age_group_id, age_group_name, val, lower, upper)]
severe[,severity:="Severe"]

## For mod-sev, need to pull draws of moderate + severe, combine, and then re-collapse

mod_draws <- get_draws("rei_id", 206, source="como", measure_id=5, location_id=loc_id, year_id=c(1990:2021),
                       age_group_id=age_ids, sex_id=3, metric_id=3, gbd_round_id=7, decomp_step="iterative",
                       version_id=1353)[cause_id==294]
mod_draws[,c("metric_id","cause_id","measure_id","rei_id","version_id"):=NULL]

sev_draws <- get_draws("rei_id", 207, source="como", measure_id=5, location_id=loc_id, year_id=c(1990:2021),
                       age_group_id=age_ids, sex_id=3, metric_id=3, gbd_round_id=7, decomp_step="iterative",
                       version_id=1353)[cause_id==294]
sev_draws[,c("metric_id","cause_id","measure_id","rei_id","version_id"):=NULL]

mod_draws <- melt(mod_draws, id.vars=c("location_id","year_id","age_group_id","sex_id"), value.name="mod")
sev_draws <- melt(sev_draws, id.vars=c("location_id","year_id","age_group_id","sex_id"), value.name="sev")

modsev_draws <- merge(mod_draws, sev_draws, by=c("location_id","year_id","age_group_id","sex_id","variable"),all=TRUE)
modsev_draws[,value:=mod+sev]
modsev_draws[,c("mod","sev") := NULL]

rm(mod_draws); rm(sev_draws)
modsev_draws <- dcast(modsev_draws, location_id + year_id + age_group_id + sex_id ~ variable)

moderate <- collapse_point(modsev_draws); rm(modsev_draws)
setnames(moderate,"mean","val")

moderate[,location_name := unique(severe$location_name)]
moderate[,sex := "Both"]
moderate <- merge(moderate,unique(severe[,.(age_group_id,age_group_name)]), by="age_group_id")
moderate[,severity := "Moderate + Severe"]

## Combine and format
data <- rbind(total, moderate, severe)
data[, age_group_name:=NULL]
data <- merge(data, age_map, by="age_group_id", all.x=TRUE)
data[,age_group_name := factor(age_group_name, levels=age_map$age_group_name)]
data <- merge(data,loc_meta[,.(location_id,lancet_label)],by="location_id",all.x=TRUE)

data[, severity := factor(severity, levels=c("Total","Moderate + Severe","Severe"))]
colors <- c("#1B9E77", "#D95F02", "#7570B3")

plot1 <- ggplot(data=data,aes(x=year_id,y=val,ymin=lower,ymax=upper,color=severity,fill=severity)) + 
            geom_line(size=1) + 
            geom_ribbon(alpha=0.2, color=NA) + 
            facet_wrap(~age_group_name, ncol=7) +
            scale_color_manual(values=colors) + 
            scale_fill_manual(values=colors) +
            theme_bw() +
            scale_x_continuous(limits = c(1990,2021), expand = c(0.025, 0.025)) +
            labs(title=unique(data$lancet_label),
                 subtitle="Anaemia Prevalence - Both Sexes",
                 x="Year", y="Prevalence (%)") +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  axis.text.x = element_text(size = 8, angle=45, hjust=1, vjust=1),
                  strip.background =element_rect(fill="white")) + 
            guides(color=guide_legend(nrow=1))


## Causal Attribution by Age
inmap <- read.xlsx('FILEPATH/in_out_meid_map_2020.xlsx')
outmap <- read.xlsx('FILEPATH/in_out_meid_map_2020.xlsx', sheet = 'out_meids')
allmap <- merge(inmap, outmap, by='subtype') %>%
  arrange(order)
cause_name_map <- fread("FILEPATH/cause_name_map.csv")

customColors <- c("#0006FD", "#0072B2", "#0080FF", "#56B4E9", "#004867", "#42F4F4", "#21B2B2", "#0B6666", 
                  "#FF00F9", "#800080", "#DB0026", 
                  "#AFA06D", "#EBDB9E", "#D4CD8A", "#EDE587", "#E3C25D", "#4C381E", "#A38446", "#997423", "#DBCF21", "#FFF644", "#FFFF25", "#F3F707", 
                  "#00B22B", "#009E73",   
                  "#E69F00", "#F77500", "#D55E00", "#FF0000", "#F99363", 
                  "#4F4F4C", "#70706D", "#000000", "#DBDAD0",
                  "#330066", "#9933CC", "#CC66FF")

df <- fread(paste0("FILEPATH/",loc_id,".csv"))
endo_other <- df[cause_name=="Endocrine - Other"]
df <- rbind(df[!cause_name=="Endocrine - Other"], endo_other)
df$cause_name <- factor(df$cause_name, levels = unique(df$cause_name))
df <- merge(df, age_map, by = "age_group_id", all.x=TRUE)
df[, sex:=ifelse(sex_id==1,"Males","Females")]
df[, age_group_name := NULL]
df <- merge(df, age_map, by="age_group_id",all.x=TRUE)
df[, age_group_name:=factor(age_group_name,levels=c("Early Neonatal","Late Neonatal","1 to 5 months","6 to 11 months",
                                                    "12 to 23 months","2 to 4","5 to 9","10 to 14","15 to 19",
                                                    "20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                                    "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69",
                                                    "70 to 74","75 to 79","80 to 84","85 to 89","90 to 94",
                                                    "95 plus","All Ages","Age-standardised"))]

df <- df[year_id==2021, .(location_id,sex_id,sex,age_group_name,total_prevalence,cause_name)]
df <- merge(df,cause_name_map,by="cause_name",all.x=TRUE)
df[, cause_name_short := factor(cause_name_short, levels=unique(cause_name_map$cause_name_short))]

# Global by sex
plot2 <- ggplot(df, aes(x=age_group_name, y = total_prevalence, fill = cause_name_short)) +
                geom_bar(stat="identity") + theme_minimal() + facet_wrap(~sex, ncol=1) +
                labs(fill="Cause", y="Prevalence (%)",x="Age",
                     title=" ",
                     subtitle="Cause-Specific Anaemia Prevalence - 2021") + 
                scale_fill_manual(NULL, values=customColors, drop=F, guide=guide_legend(ncol=1)) +
                scale_y_continuous(expand = c(0, 0)) +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size = 8, angle=45, hjust=1, vjust=1, color="black"),
                      axis.text.y = element_text(color="black")) +
                guides(fill=guide_legend(keyheight=0.6, ncol=1))

### Arrow plots
## Pull data first to set global cut-offs

data3m <- get_outputs("rei", rei_id=192, measure_id=3, location_id=loc_id, year_id=c(1990,2021),
                      age_group_id=22, sex_id=1, metric_id=3, gbd_round_id=7, decomp_step="iterative",
                      compare_version_id=7846, cause_id="lvl3")[,.(location_id, location_name, year_id, sex_id, sex, acause, cause_name, val)]
data3m <- data3m[!(acause %in% c("gyne","maternal"))]
data3m[,val:=val*100000]
data3m[year_id==2021, year_id:=2020]

data3f <- get_outputs("rei", rei_id=192, measure_id=3, location_id=loc_id, year_id=c(1990,2021),
                      age_group_id=22, sex_id=2, metric_id=3, gbd_round_id=7, decomp_step="iterative",
                      compare_version_id=7846, cause_id="lvl3")[,.(location_id, location_name, year_id, sex_id, sex, acause, cause_name, val)]
data3f[,val:=val*100000]
data3f[year_id==2021, year_id:=2020] 

data3mf <- rbind(data3m, data3f)

limit_vector <- round(quantile(data3mf$val, probs = seq(0, 1, .1)),2)
limit_vector[1] <- limit_vector[1]-0.1
limit_vector[11] <- limit_vector[11]+0.1
if (limit_vector[1] < 0) limit_vector[1] <- 0
colors <- rev(brewer.pal(10,"RdYlBu"))


## Arrow plot males data prep
years <- c(1990,2020)

data3m[, reverse_rank := rank(val, ties.method="first"), by=c("location_id","year_id","sex_id")]
data3m[, cause_rank := dense_rank(desc(reverse_rank)), by=c("location_id","year_id","sex_id")]

data3m[cause_name=="Hemoglobinopathies and hemolytic anemias",
      cause_name:="Haemoglobinopathies and \nhaemolytic anaemias"]
data3m[cause_name=="Other neglected tropical diseases",
      cause_name:="Other neglected \ntropical diseases"]
data3m[cause_name=="Other unspecified infectious diseases",
      cause_name:="Other unspecified \ninfectious diseases"]
data3m[cause_name=="Upper digestive system diseases",
      cause_name:="Upper digestive \nsystem diseases"]
data3m[cause_name=="Endocrine, metabolic, blood, and immune disorders",
      cause_name:="Endocrine, metabolic, blood, \nand immune disorders"]
data3m[cause_name=="Cirrhosis and other chronic liver diseases",
      cause_name:="Cirrhosis and other \nchronic liver diseases"]
data3m[cause_name=="Intestinal nematode infections",
      cause_name:="Intestinal nematode \ninfections"]

colors <- rev(brewer.pal(10,"RdYlBu"))

n <- length(limit_vector) - 1
labels <- c()
for (g in 1:n) {
  labels <- c(labels, paste0(limit_vector[g], "-", limit_vector[g+1]))
}

data3m[, colorlab := NA]
for (g in 1:n) {
  ii <- (data3m$val >= limit_vector[g] & data3m$val <= limit_vector[g+1]) 
  data3m$colorlab[ii] <- colors[g]
}

data3m[, colorlab := factor(colorlab,levels = paste(as.character(colors)))]


graph_years <- c(1990,2005,2020)
year_colors <- ifelse(endsWith(as.character(graph_years), '5'), 'white', 'black')

levels(data3m$year_id) <- c(levels(data3m$year_id), 2005)
data3m$year_id <- factor(data3m$year_id, levels=graph_years)

start_years <- head(years,-1)
end_years <- tail(years,-1)

start_years_data3m <- data3m %>%
  filter(year_id %in% start_years) %>%
  mutate(year_id = as.numeric(as.character(year_id))) %>%
  mutate(year_id = year_id + 15) %>%
  mutate(year_id = factor(year_id, levels=seq(1990,2020,5))) %>%
  arrange(year_id, cause_name)

end_years_data3m <- data3m %>%
  filter(year_id %in% end_years) %>%
  arrange(year_id, cause_name)

rank_diff <- start_years_data3m$reverse_rank - end_years_data3m$reverse_rank
rank_decrease <- factor(rank_diff > 0)
start_years_data3m$rank_decrease <- rank_decrease
end_years_data3m$rank_decrease <- rank_decrease

line_data3m <- rbind(start_years_data3m, end_years_data3m) %>%
  arrange(year_id, cause_name) 

line_data3m_1990_2020 <- line_data3m[line_data3m$year_id %in% c(2005,2020),]


## Arrow plot females data prep
years <- c(1990,2020)

data3f[, reverse_rank := rank(val, ties.method="first"), by=c("location_id","year_id","sex_id")]
data3f[, cause_rank := dense_rank(desc(reverse_rank)), by=c("location_id","year_id","sex_id")]

data3f[cause_name=="Hemoglobinopathies and hemolytic anemias",
       cause_name:="Haemoglobinopathies and \nhaemolytic anaemias"]
data3f[cause_name=="Other neglected tropical diseases",
       cause_name:="Other neglected \ntropical diseases"]
data3f[cause_name=="Other unspecified infectious diseases",
       cause_name:="Other unspecified \ninfectious diseases"]
data3f[cause_name=="Upper digestive system diseases",
       cause_name:="Upper digestive \nsystem diseases"]
data3f[cause_name=="Endocrine, metabolic, blood, and immune disorders",
       cause_name:="Endocrine, metabolic, blood, \nand immune disorders"]
data3f[cause_name=="Cirrhosis and other chronic liver diseases",
       cause_name:="Cirrhosis and other \nchronic liver diseases"]
data3f[cause_name=="Intestinal nematode infections",
       cause_name:="Intestinal nematode \ninfections"]


n <- length(limit_vector) - 1
labels <- c()
for (g in 1:n) {
  labels <- c(labels, paste0(limit_vector[g], "-", limit_vector[g+1]))
}

data3f[, colorlab := NA]
for (g in 1:n) {
  ii <- (data3f$val >= limit_vector[g] & data3f$val <= limit_vector[g+1]) 
  data3f$colorlab[ii] <- colors[g]
}

data3f[, colorlab := factor(colorlab,levels = paste(as.character(colors)))]


graph_years <- c(1990,2005,2020)
year_colors <- ifelse(endsWith(as.character(graph_years), '5'), 'white', 'black')

levels(data3f$year_id) <- c(levels(data3f$year_id), 2005)
data3f$year_id <- factor(data3f$year_id, levels=graph_years)

start_years <- head(years,-1)
end_years <- tail(years,-1)

start_years_data3f <- data3f %>%
  filter(year_id %in% start_years) %>%
  mutate(year_id = as.numeric(as.character(year_id))) %>%
  mutate(year_id = year_id + 15) %>%
  mutate(year_id = factor(year_id, levels=seq(1990,2020,5))) %>%
  arrange(year_id, cause_name)

end_years_data3f <- data3f %>%
  filter(year_id %in% end_years) %>%
  arrange(year_id, cause_name)

rank_diff <- start_years_data3f$reverse_rank - end_years_data3f$reverse_rank
rank_decrease <- factor(rank_diff > 0)
start_years_data3f$rank_decrease <- rank_decrease
end_years_data3f$rank_decrease <- rank_decrease

line_data3f <- rbind(start_years_data3f, end_years_data3f) %>%
  arrange(year_id, cause_name) 

line_data3f_1990_2020 <- line_data3f[line_data3f$year_id %in% c(2005,2020),]

## Arrow plot plot
data3mf <- rbind(data3m, data3f)

plot3m <- ggplot(data3mf[sex=="Male"], aes(x=year_id, y=reverse_rank)) +
  geom_tile(aes(fill=colorlab), colour='black') + 
  geom_text(aes(label = cause_name), size=2.5, lineheight=0.7) + 
  geom_line(data=line_data3m_1990_2020, 
            aes(x=year_id, y=reverse_rank, group=cause_name, linetype=rank_decrease),
            position=position_nudge(x=-.5)) +
  scale_x_discrete(expand=c(0,0), limits=levels(data3f$year_id), labels=c("1990","2005","2021"), position='top') +
  scale_y_discrete(expand = expansion(mult = c(0, .01))) +
  scale_fill_manual(values=colors, labels=labels, drop=FALSE) +
  theme(text=element_text(size=12),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(colour=year_colors),
        plot.title = element_text(size=rel(3), hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        legend.position="bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title.align=0.5,
        legend.text=element_text(size=8))+
  labs(fill="YLDs per 100,000 \npopulation",
       subtitle="Males") +
  guides(linetype = FALSE)


plot3f <- ggplot(data3mf[sex=="Female"], aes(x=year_id, y=reverse_rank)) +
                geom_tile(aes(fill=colorlab), colour='black') + 
                geom_text(aes(label = cause_name), size=2.5, lineheight=0.7) + 
                geom_line(data=line_data3f_1990_2020, 
                          aes(x=year_id, y=reverse_rank, group=cause_name, linetype=rank_decrease),
                          position=position_nudge(x=-.5)) +
                scale_x_discrete(expand=c(0,0), limits=levels(data3f$year_id), labels=c("1990","2005","2021"), position='top') +
                scale_y_discrete(expand = expansion(mult = c(0, .01))) +
                scale_fill_manual(values=colors, labels=labels, drop=FALSE) +
                theme(text=element_text(size=12),
                      panel.background = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.title = element_blank(),
                      axis.text.x = element_text(colour=year_colors),
                      plot.title = element_text(size=rel(3), hjust=0.5),
                      plot.subtitle = element_text(hjust=0.5),
                      legend.position="bottom",
                      legend.background = element_blank(),
                      legend.box.background = element_rect(colour = "black"),
                      legend.title.align=0.5,
                      legend.text=element_text(size=8))+
                labs(fill="YLDs per 100,000 \npopulation",
                     subtitle="Females") +
                guides(linetype = FALSE)



# Combine!
figa <- ggarrange(plot3m, plot3f, ncol=2, common.legend=TRUE, legend="bottom")
figb <- ggarrange(plot2, figa, ncol=1)
figc <- ggarrange(plot1, figb, ncol=2)
