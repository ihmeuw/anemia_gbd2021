### Anemia Manuscript Epi Transition Analysis

rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "FILEPATH"
  h_root <- "FILEPATH"
  central_lib <- "FILEPATH"
} else {
  j_root <- "FILEPATH"
  h_root <- "FILEPATH"
  central_lib <- "FILEPATH"
}

# load packages 
library(data.table)
library(magrittr)
library(ggplot2)
library(mrbrt002, lib.loc = "FILEPATH")

# Directories -------------------------------------------------------------
out_dir <- "FILEPATH/"
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_covariate_estimates.R")
source("FILEPATH/get_outputs.R")
source("FILEPATH/get_population.R")
source("FILEPATH/get_age_weights.R")

age_weights <- get_age_weights(gbd_round_id = 7)
hierarchy <- get_location_metadata(35, gbd_round_id = 7)
loc_meta <- get_location_metadata(35, gbd_round_id=7)

logit <- function(x){log(x/(1-x))}
invlogit <- function(x){exp(x)/(1+exp(x))}

custom.col.r <- c("Central Asia" = "#771155",
                  "Central Europe" = "#AA4488",
                  "Eastern Europe" = "#CC99BB",
                  "Australasia" = "#117777",
                  "High-income Asia Pacific" = "#44AAAA",
                  "High-income North America" = "#77CCCC",
                  "Southern Latin America" = "#117744",
                  "Western Europe" = "#44AA77",
                  "Andean Latin America" = "#771122", 
                  "Caribbean" = "#993344",
                  "Central Latin America" = "#BB5566",
                  "Tropical Latin America" = "#DD7788",
                  "North Africa and Middle East" = "#777711", 
                  "South Asia" = "#000080", 
                  "East Asia" = "#114477", 
                  "Oceania" = "#4477AA",
                  "Southeast Asia" = "#77AADD",
                  "Central Sub-Saharan Africa" = "#774411", 
                  "Eastern Sub-Saharan Africa" = "#996633",
                  "Southern Sub-Saharan Africa" = "#BB8855",
                  "Western Sub-Saharan Africa" = "#DDAA77")


# Read in anemia draws and calculate means --------------------------------

anemia_dt <- get_outputs("rei", rei_id = 192, year_id = 1990:2021, 
                          measure_id = c(3,5), metric_id = 3, 
                          age_group_id = "all", sex_id = c(1,2), 
                          location_id = hierarchy[most_detailed == 1, location_id],
                          gbd_round_id = 7, decomp_step = "iterative", compare_version_id = 7846)
 
write.csv(anemia_dt, file.path(out_dir, "data.csv"), row.names = F)

anemia_dt <- fread(file.path(out_dir, "data.csv"))
anemia_dt <- anemia_dt[year_id %in% c(1990,1995,2000,2005,2010,2015,2021)]


# merge on sdi
sdi <- get_covariate_estimates(881, gbd_round_id = 7, decomp_step = "iterative")

anemia_dt <- merge(anemia_dt, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))

anemia_dt <- merge(anemia_dt, loc_meta[, .(location_id, region_name, super_region_name)], all.x=T, by = "location_id")

# run age-, sex-, measure-specific splines
out_dt <- data.table()

pdf(file.path(out_dir, "spline_plots.pdf"), height = 7.5, width = 10)

for(mid in unique(anemia_dt$measure_id)){
  for(sid in unique(anemia_dt$sex_id)){
    for(agid in unique(anemia_dt$age_group_id)){
      
      data_dt <- anemia_dt[age_group_id == agid & sex_id == sid & measure_id == mid]
      
      # replace SE with the same value for all locations
      data_dt[, se := 1]
      
      # use offset before logit
      data_dt[val<1e-7, val := 1e-7]
      data_dt[val>(1-1e-7), val := (1-1e-7)]
      data_dt[, logit_val := logit(val)]
      
      # run MRBRT spline
      dat_1 <- MRData()
      dat_1$load_df(
        data = data_dt,  col_obs = "logit_val", col_obs_se = "se",
        col_covs = list("sdi"), col_study_id = "location_id"
      )
      
      
      model <- MRBRT(data = dat_1,
                     cov_models = list(LinearCovModel("sdi", 
                                                      use_spline = TRUE,
                                                      use_re = FALSE,
                                                      spline_r_linear = TRUE,
                                                      spline_l_linear = TRUE),
                                       LinearCovModel("intercept", 
                                                      use_re = FALSE)))
      
      model$fit_model()
      
      pred_dt <- rbind(data_dt, data.table(sdi = c(0,1)), fill = T)
      
      pred_dt <- pred_dt[order(sdi)]
      
      dat_pred <- MRData()
      
      dat_pred$load_df(
        data = pred_dt,
        col_covs=list("sdi")
      )
      
      pred_dt$expected <- model$predict(dat_pred) %>% invlogit
      
      out_dt <- rbind(out_dt, pred_dt[!is.na(location_id)])
      
      print(paste("Done with", mid, sid, agid))
    }
    
    gg <- ggplot(out_dt[measure_id == mid & sex_id == sid], aes(y=val, x=sdi))+
      geom_point(data = anemia_dt[measure_id == mid & sex_id == sid & year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2021)], 
                 aes(color = region_name), alpha = 0.2)+
      geom_line(aes(y = expected))+
      theme_bw()+
      scale_color_manual(values = custom.col.r)+
      labs(title = paste0("Anemia ", unique(data_dt$measure_name), " among ", 
                          unique(data_dt$sex), "s, 1990-2021"),
           y = paste(unique(data_dt$measure_name), unique(data_dt$metric_name)), 
           x = "SDI",
           color = "")+
      facet_wrap(~age_group_name)+
      theme(legend.position="bottom")
    
    print(gg)
  }
}

dev.off()

write.csv(out_dt, file.path(out_dir, "observed_and_expected.csv"), row.names = F)

out_dt <- fread(file.path(out_dir, "observed_and_expected.csv"))

out_dt <- out_dt[, .(location_id, age_group_id, year_id, sex_id, measure_id, metric_id, expected, observed = val, measure_name, metric_name)]


# Create aggregates -------------------------------------------------------

# merge on populations

population <- get_population(gbd_round_id = 7, decomp_step = "iterative",
                             location_id = unique(out_dt$location_id), 
                             age_group_id = unique(out_dt$age_group_id),
                             sex_id = unique(out_dt$sex_id),
                             year_id = unique(out_dt$year_id))

population[, run_id := NULL]

out_dt <- merge(out_dt, population, by = c("location_id", "age_group_id", "year_id", "sex_id"), all.x=T)

# merge on age weights
out_dt <- merge(out_dt, age_weights, by = "age_group_id", all.x=T)

# Aggreagate up location hierarchy

for(loc in loc_meta[most_detailed == 0, location_id]){
  
  if(loc == 1){
    agg_dt <- copy(out_dt)
  }else{
    child_locs <- loc_meta[most_detailed == 1 & (parent_id == loc | grepl(paste0(",", loc, ","), path_to_top_parent)), location_id]
    
    agg_dt <- out_dt[location_id %in% child_locs] 
  }
  
  agg_dt <- agg_dt[, .(location_id = loc, expected = weighted.mean(expected, w = population), observed = weighted.mean(observed, w = population),
                       population = sum(population)),
                   by = c("age_group_id", "year_id", "sex_id", "measure_id", "metric_id", "measure_name", "metric_name", "age_group_weight_value")]
  
  out_dt <- rbind(out_dt, agg_dt, use.names = T)
  
}


# Aggregate to desired age groups

ages <- list("Under 1" = c(2,3,388,389),
             "Under 5" = c(2,3,388,389,238,34),
             "15 - 49" = 8:14,
             "50 - 69" = 15:18,
             "70+" = c(19,20, 20:32, 235), 
             "All Ages" = unique(out_dt$age_group_id),
             "Age-standardized" = unique(out_dt$age_group_id))

sum_dt <- data.table()

for(i in 1:length(ages)){
  
  agg_dt <- out_dt[age_group_id %in% ages[[i]]]
  
  if(names(ages)[i]=="Age-standardized"){
    agg_dt[, weight := age_group_weight_value]
  }else{
    agg_dt[, weight := population]
  }
  
  agg_dt <- agg_dt[, .(expected = weighted.mean(expected, w = weight), observed = weighted.mean(observed, w = weight), age_group_name = names(ages)[i]),
                   by = c("location_id", "year_id", "measure_id", "metric_id", "measure_name", "metric_name")]
  
  sum_dt <- rbind(sum_dt, agg_dt, use.names = T)
  
}


# merge on location names
sum_dt <- merge(sum_dt, loc_meta[, .(location_id, location_name, ihme_loc_id, level, super_region_name, region_name)], by = "location_id", all.x=T)

sum_dt[, ratio := observed/expected]

sum_dt <- merge(sum_dt, sdi[,.(location_id, year_id, sdi = mean_value)], all.x=T, by = c("location_id", "year_id"))


write.csv(sum_dt, file.path(out_dir, "summary_results.csv"), row.names = F)


# Generate AROC -----------------------------------------------------------
years <- list(c(1990, 1995), c(1995, 2000), c(2000, 2005), c(2005, 2010), 
              c(2010, 2015), c(2015, 2021), c(1990, 2021), c(2000, 2021))

change_dt <- data.table()

for(i in 1:length(years)){
  year_start <- years[[i]][1]
  year_end <- years[[i]][2]
  
  aroc_dt <- merge(sum_dt[year_id == year_start], sum_dt[year_id == year_end],
                   by = c("location_id", "measure_id", "metric_id", "measure_name", 
                          "metric_name", "age_group_name", "location_name", "ihme_loc_id", 
                          "level", "super_region_name", "region_name"))
  
  aroc_dt[, aroc:=log(ratio.y/ratio.x)/(year_end - year_start)]
  
  aroc_dt <- aroc_dt[, c("location_id", "measure_id", "metric_id", "measure_name", 
                         "metric_name", "age_group_name", "location_name", "ihme_loc_id", 
                         "level", "super_region_name", "region_name", "aroc")]
  
  aroc_dt[, year_start := year_start]
  aroc_dt[, year_end := year_end]
  
  change_dt <- rbind(change_dt, aroc_dt)
  
}

write.csv(change_dt, file.path(out_dir, "aroc_results.csv"), row.names = F)



# Plot Scatters -----------------------------------------------------------

sum_dt[, region_name := factor(region_name, levels = names(custom.col.r))]

plot3a <- ggplot(sum_dt[level==2 & age_group_name=="All Ages" & measure_id==5], aes(x=sdi, y=observed*100)) + 
                  geom_line(aes(color=region_name),size=1) + 
                  geom_line(data = sum_dt[location_id %in% hierarchy[most_detailed == 1, location_id] & age_group_name == "Age-standardized" & measure_id == 5], 
                            aes(y=expected*100)) +
                  scale_color_manual(values=custom.col.r, guide=guide_legend(ncol=1)) + 
                  labs(title = "Anaemia prevalence based on SDI",
                       x = "Socio-demographic Index", y = "Prevalence (%)") + 
                  theme_classic() +
                  theme(legend.title=element_blank()) +
                  guides(color=guide_legend(keyheight=1, ncol=1))

plot3b <- ggplot(sum_dt[level==2 & age_group_name=="All Ages" & measure_id==3], aes(x=sdi, y=observed*1e5)) + 
                  geom_line(aes(color=region_name),size=1) + 
                  geom_line(data = sum_dt[location_id %in% hierarchy[most_detailed == 1, location_id] & age_group_name == "Age-standardized" & measure_id == 3], 
                            aes(y=expected*1e5)) +
                  scale_color_manual(values=custom.col.r, guide=guide_legend(ncol=1)) + 
                  labs(title = "Anaemia YLDs based on SDI",
                       x = "Socio-demographic Index", y = "YLDs (rate per 100,000 population)") + 
                  theme_classic() +
                  theme(legend.title=element_blank()) +
                  guides(color=guide_legend(keyheight=1, ncol=1))


# Plot Map ---------------------------------------------------------------

source("FILEPATH/map_national_and_subnational.R")

map_dt <- sum_dt[measure_id == 3 & age_group_name == "All Ages" & year_id == 2021]

breaks <- c(0.20, 0.50, 0.75, 0.90, 1.0, 1.5, 2.5, 3.5, 5.5)
plot3c <- map_national_and_subnational(data=map_dt, map.var="ratio", subnat.style="in.map", subnat.level="public",
                                       scale="cat", breaks=breaks, labels=NULL, unit.type="count",
                                       legend.title="Observed/Expected\nYLD rate", 
                                       col.rev=TRUE, legend.title.size=8)

# Combine
fig3a <- ggarrange(plot3a, plot3b, ncol=2, common.legend=TRUE, legend="right", labels=c("A","B"))
fig3b <- ggarrange(plot3c, ncol=1, labels=c("C"))
fig3 <- ggarrange(fig3a, fig3b, nrow=2)
fig3_final <- annotate_figure(fig3, fig.lab = "Epidemiologic transition of all-ages anaemia burden on the basis of socio-demographic index (SDI)", 
                              fig.lab.pos="bottom.left", fig.lab.face="bold")
