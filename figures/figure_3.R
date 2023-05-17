### Anemia Manuscript Figure 3

rm(list=ls())

## Libraries and functions
library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_outputs.R")

## Metadata
locs <- get_location_metadata(35, gbd_round_id=7)
female_causes <- c("Maternal disorders","Gynecological diseases")


## Pull data
df1990 <- get_outputs("rei", rei_id=192, measure_id=3, location_id=locs[level<=1,location_id], year_id=c(1990),
                  age_group_id=22, sex_id=c(1,2), metric_id=3, gbd_round_id=7, decomp_step="iterative",
                  compare_version_id=7846, cause_id="lvl3")[,.(location_id, location_name, year_id, sex_id, sex, acause, cause_name, val)]

df2020 <- get_outputs("rei", rei_id=192, measure_id=3, location_id=locs[level<=1,location_id], year_id=c(2021),
                  age_group_id=22, sex_id=c(1,2), metric_id=3, gbd_round_id=7, decomp_step="iterative",
                  compare_version_id=7846, cause_id="lvl3")[,.(location_id, location_name, year_id, sex_id, sex, acause, cause_name, val)]

df <- rbind(df1990, df2020)

df <- df[!(sex_id==1 & cause_name %in% c("Maternal disorders","Gynecological diseases")),]
df[,val:=val*100000]
df[, reverse_rank := rank(val, ties.method="first", na.last=FALSE), by=c("location_id","year_id","sex_id")]
df[, cause_rank := dense_rank(desc(reverse_rank)), by=c("location_id","year_id","sex_id")]

df[location_name=="Central Europe, Eastern Europe, and Central Asia", location_name:="Central Europe, \nEastern Europe, \nand Central Asia"]
df[location_name=="Latin America and Caribbean", location_name:="Latin America \nand Caribbean"]
df[location_name=="North Africa and Middle East", location_name:="North Africa and \nMiddle East"]
df[location_name=="Southeast Asia, East Asia, and Oceania", location_name:="Southeast Asia, \nEast Asia, \nand Oceania"]
df[location_name=="Sub-Saharan Africa", location_name:="Sub-Saharan \nAfrica"]
df[,location_name := factor(location_name, levels=c("Global","Central Europe, \nEastern Europe, \nand Central Asia",
                                                    "High-income", "Latin America \nand Caribbean",
                                                    "North Africa and \nMiddle East", "South Asia",
                                                    "Southeast Asia, \nEast Asia, \nand Oceania",
                                                    "Sub-Saharan \nAfrica"))]

df[cause_name=="Hemoglobinopathies and hemolytic anemias",
   cause_name:="Haemoglobinopathies and \nhaemolytic anaemias"]
df[cause_name=="Other neglected tropical diseases",
   cause_name:="Other neglected \ntropical diseases"]
df[cause_name=="Other unspecified infectious diseases",
   cause_name:="Other unspecified \ninfectious diseases"]
df[cause_name=="Upper digestive system diseases",
   cause_name:="Upper digestive \nsystem diseases"]
df[cause_name=="Endocrine, metabolic, blood, and immune disorders",
   cause_name:="Endocrine, metabolic, blood, \nand immune disorders"]
df[cause_name=="Cirrhosis and other chronic liver diseases",
   cause_name:="Cirrhosis and other \nchronic liver diseases"]
df[cause_name=="Intestinal nematode infections",
   cause_name:="Intestinal nematode \ninfections"]

limit_vector <- round(quantile(df$val, probs = seq(0, 1, .1)),2)
limit_vector[2] <- 0.05
limit_vector[11] <- 1702.25
colors <- rev(brewer.pal(10,"RdYlBu"))

n <- length(limit_vector) - 1
labels <- c()
for (g in 1:n) {
  labels <- c(labels, paste0(limit_vector[g], "-", limit_vector[g+1]))
}

df[, colorlab := NA]
for (g in 1:n) {
  ii <- (df$val >= limit_vector[g] & df$val <= limit_vector[g+1]) 
  df$colorlab[ii] <- colors[g]
}

df[, colorlab := factor(colorlab,levels = paste(as.character(colors)))]

# 1990 Males
data <- df[year_id==1990 & sex_id==1 & !(cause_name %in% female_causes)]
sort_order <- data[location_id==1]
sort_order <- sort_order[order(cause_rank)]
data[, cause_name := factor(cause_name, levels=unique(sort_order$cause_name))]

plot_4a <-ggplot(data, aes(x=location_name, y=cause_name)) +
  geom_tile(aes(fill = colorlab), color="black", size=1) + theme_classic() +
  geom_text(aes(label = cause_rank), size=6) + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values=colors, labels=labels) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(x=element_blank(), y=element_blank(),
       subtitle="Males",
       fill="YLDs per 100,000 \npopulation") +
  theme(legend.position=NULL,
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.subtitle=element_text(size=16))

# 1990 Females
data <- df[year_id==1990 & sex_id==2]
sort_order <- data[location_id==1]
sort_order <- sort_order[order(cause_rank)]
data[, cause_name := factor(cause_name, levels=unique(sort_order$cause_name))]

plot_4b <- ggplot(data, aes(x=location_name, y=cause_name)) +
  geom_tile(aes(fill = colorlab), color="black", size=1) + theme_classic() +
  geom_text(aes(label = cause_rank), size=6) + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values=colors, labels=labels) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(x=element_blank(), y=element_blank(),
       subtitle="Females",
       fill="YLDs per 100,000 \npopulation") +
  theme(legend.position=NULL,
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.subtitle=element_text(size=16))


# 2020 Males
data <- df[year_id==2021 & sex_id==1 & !(cause_name %in% female_causes)]
sort_order <- data[location_id==1]
sort_order <- sort_order[order(cause_rank)]
data[, cause_name := factor(cause_name, levels=unique(sort_order$cause_name))]

plot_4c <- ggplot(data, aes(x=location_name, y=cause_name)) +
  geom_tile(aes(fill = colorlab), color="black", size=1) + theme_classic() +
  geom_text(aes(label = cause_rank), size=6) + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values=colors, labels=labels) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(x=element_blank(), y=element_blank(),
       subtitle="Males",
       fill="YLDs per 100,000 \npopulation") +
  theme(legend.position="bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title.align=0.5,
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.subtitle=element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

# 2020 Females
data <- df[year_id==2021 & sex_id==2]
sort_order <- data[location_id==1]
sort_order <- sort_order[order(cause_rank)]
data[, cause_name := factor(cause_name, levels=unique(sort_order$cause_name))]

plot_4d <- ggplot(data, aes(x=location_name, y=cause_name)) +
  geom_tile(aes(fill = colorlab), color="black", size=1) + theme_classic() +
  geom_text(aes(label = cause_rank), size=6) + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values=colors, labels=labels) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(x=element_blank(), y=element_blank(),
       subtitle="Females",
       fill="YLDs per 100,000 \npopulation") +
  theme(legend.position="bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title.align=0.5,
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.subtitle=element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

# Combine
fig4a <- ggarrange(plot_4a, plot_4b, ncol=2, legend="none",labels=c("1990",""))
fig4b <- ggarrange(plot_4c, plot_4d, ncol=2, common.legend=TRUE, legend="bottom", labels=c("2021",""))
fig4 <- ggarrange(fig4a, fig4b, nrow=2)
fig4_final <- annotate_figure(fig4, fig.lab = "Figure 3 Causes of anaemia by YLD ranking", fig.lab.pos="bottom.left", fig.lab.face="bold", fig.lab.size=14)
