#install.packages("XYZ")
require(dplyr)
require(Thermimage)
require(ggplot2)
require(xts)
require(lubridate)
require(timetk)
require(tidyverse)
require(chron)
require(dplyr)
require(ggthemes)
require(ggplot2)
require(plotrix)
require(plotfunctions)
require(raster)
require(fANCOVA)
require(rgeos)
require(dplyr)

#######################################
####DAILY MEAN MAX MIN TEMPERATURES####
#######################################

# CHANGE PLOT NAME
setwd("FILEPATH")
plot_name = "las_17"

# ADAPT FLEXIBLY BASED ON FILE COLUMN NAMES
header <- c("egal", "date", "egal2", "t_soil", "t_surface", "t_air", "egal3", "egal4", "egal5")

# CHANGE FILE NAME
microclimate_las1 <- read.csv(paste("FILEPATH/",plot_name,"/data_94193801_0.csv", sep=""),
                              header = FALSE, 
                              col.names =  header, 
                              sep = ";")

microclimate_las1 <- microclimate_las1[,c("date", "t_soil", "t_surface", "t_air")]

microclimate_las1$time <- strptime(microclimate_las1$date, "%Y.%m.%d %H:%M")
microclimate_las1$date <- as.Date(microclimate_las1$time)
microclimate_las1$hour = times(strftime(microclimate_las1$time, format="%T"))

#write.csv(microclimate_las1, 
          #paste("E:/Microclimate/Agroecology_data/", plot_name, "/", plot_name, "_microclimate.csv", sep = ""))
#microclimate_las1$hour <-  as.numeric(microclimate_las1$hour)

daily_microclimate <- microclimate_las1 %>%
  group_by(date) %>%
  summarize(tmean_soil = mean(t_soil),
            tmax_soil = max(t_soil),
            tmin_soil = min(t_soil),
            tsd_soil = sd(t_soil),
            tmean_surface = mean(t_surface),
            tmax_surface = max(t_surface),
            tmin_surface = min(t_surface),
            tsd_surface = sd(t_surface),
            tmean_air = mean(t_air),
            tmax_air = max(t_air),
            tmin_air = min(t_air),
            tsd_air = sd(t_air))

daily_microclimate["las_name"] <- plot_name
daily_microclimate <- daily_microclimate[-c(266:271), ]
# EXPORT 
write.csv(daily_microclimate, 
          paste("FILEPATH", plot_name, "_daily_summary.csv", sep = ""))

# COMBINE ALL PLOTS

# create a list of all summary metric files
my_files <- list.files("FILEPATH/daily_summary", pattern = "\\.csv$")
# adapt wd for lapply
setwd("FILEPATH/daily_summary")
# read all files
sum_metric_list <- lapply(my_files, read.csv)
# combine all plots to a single file
all_sum_metrics <- bind_rows(sum_metric_list)
all_sum_metrics <- all_sum_metrics %>% select(-X)

write.csv(all_sum_metrics, "FILEPATH/daily_summary/all_plots_daily_summary.csv")



#######################################
####PLOTTING####
#######################################

# READ IN
stats_plus_micro <- read.csv("FILEPATH/temp_vs_metric/stats_plus_micro.csv")

# FORMAT AS DATETIME FOR PLOT
stats_plus_micro$date <- as.Date(stats_plus_micro$date, "%Y-%m-%d")

scale = c("#a1dab4", "#41b6c4", "#41b6c4", "#2c7fb8", "#FF6666")
# PLOT 
ggplot(data = stats_plus_micro, aes(x=date, y=tmean_air, color = pad_med, group = las_name)) +
  geom_line(size = 0.7)+
  #scale_color_manual(values = scale)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text.x=element_text(angle=50,hjust=0.5,vjust=0.5))+
  labs(title = "Mean Air Temperature in *C",
       subtitle = "",
       caption = "",
       fill = "")+
  geom_vline(xintercept = "")+
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b %d")+
  scale_y_continuous("Temperature in *C")
  scale_fill_manual(values=c("#0C7BDC", "#FFC20A"))

  
  
#######################################
####DATA PREPARATION: SUPERFLUOUS BECAUSE EXPORT####
#######################################
# READ IN PAD DATA
total.pad.stat <- read.csv("FILEPATH/temp_vs_metric/total.pad.stat.csv")
# SUMMARIZE PER PLOT INSTEAD OF HEIGHT
plots.pad.stat <- total.pad.stat %>%
  group_by(plot_name) %>%
  summarize(las_name = first(las_name),
            pad_mean = mean(mean),
            pad_med = median(med),
            pad_max = max(max),
            pad_min = min(min),
            pad_sd = sd(sd),
            pad_n = sum(n),
            cc_percent = first(cc_percent.x),
            cc_class = first(cc_class),
            als_elevation = first(als_elevation),
            gps_elevation = first(gps_elevation),
            slope = first(slope),
            aspect = first(aspect_class),
            veg_type = first(veg_type),
            veg_class = first(class)
            )
# EXPORT 
#write.csv(plots.pad.stat, "E:/figures/temp_vs_metric/total.pad.stat.summarized.csv")

# read other metrics
all_sums_by_rows <- read.csv("FILEPATH/summary_metrics_by_row/all_summ_by_rows.csv")

#filter out mean, remove other rows and rename
#repeat for most important metrics
mean_metrics <- all_sums_by_rows %>% filter(metric == "Mean")
mean_metrics[ , c(1, 2, 12:14)] <- list(NULL)
mean_metrics <- rename(mean_metrics,
                       FHD_mean = FHD,
                       CR_mean = CR,
                       pai_mean = pai,
                       RH25_mean = RH25,
                       RH50_mean = RH50,
                       RH75_mean = RH75,
                       RH95_mean = RH95,
                       RH98_mean = RH98,
                       iris_name = plot_name)


median_metrics <- all_sums_by_rows %>% filter(metric == "Median")
median_metrics[ , c(1, 2, 12:14)] <- list(NULL)
median_metrics <- rename(median_metrics,
                       FHD_med = FHD,
                       CR_med = CR,
                       pai_med = pai,
                       RH25_med = RH25,
                       RH50_med = RH50,
                       RH75_med = RH75,
                       RH95_med = RH95,
                       RH98_med = RH98,
                       iris_name = plot_name)

sd_metrics <- all_sums_by_rows %>% filter(metric == "Std. Deviation")
sd_metrics[ , c(1, 2, 12:14)] <- list(NULL)
sd_metrics <- rename(sd_metrics,
                         FHD_sd = FHD,
                         CR_sd = CR,
                         pai_sd = pai,
                         RH25_sd = RH25,
                         RH50_sd = RH50,
                         RH75_sd = RH75,
                         RH95_sd = RH95,
                         RH98_sd = RH98,
                         iris_name = plot_name)

min_metrics <- all_sums_by_rows %>% filter(metric == "Min")
min_metrics[ , c(1, 2, 12:14)] <- list(NULL)
min_metrics <- rename(min_metrics,
                     FHD_min = FHD,
                     CR_min = CR,
                     pai_min = pai,
                     RH25_min = RH25,
                     RH50_min = RH50,
                     RH75_min = RH75,
                     RH95_min = RH95,
                     RH98_min = RH98,
                     iris_name = plot_name)

max_metrics <- all_sums_by_rows %>% filter(metric == "Max")
max_metrics[ , c(1, 2, 12:14)] <- list(NULL)
max_metrics <- rename(max_metrics,
                      FHD_max = FHD,
                      CR_max = CR,
                      pai_max = pai,
                      RH25_max = RH25,
                      RH50_max = RH50,
                      RH75_max = RH75,
                      RH95_max = RH95,
                      RH98_max = RH98,
                      iris_name = plot_name)
# MERGE
df_list <- list(mean_metrics, median_metrics, sd_metrics, min_metrics, max_metrics)
merged_df <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "iris_name"), df_list)
# EXPORT
# write.csv(merged_df, "E:/figures/temp_vs_metric/other_metrics_sorted.csv")

# finishing touches, deleting index columns, renaming, merging and exporting
all_sum_metrics <- read.csv("FILEPATH/daily_summary/all_plots_daily_summary.csv")
total_pad_stat <- read.csv("FILEPATH/temp_vs_metric/total.pad.stat.summarized.csv")
other_metrics_sorted <- read.csv("FILEPATH/temp_vs_metric/other_metrics_sorted.csv")
other_metrics_sorted <- rename(other_metrics_sorted, las_name = iris_name )
other_metrics_sorted <- other_metrics_sorted[,-c(1)]
merged_stats <- merge(total_pad_stat, other_metrics_sorted, all=TRUE, by = "las_name")
merged_stats <- merged_stats[,-c(2)]
stats_plus_micro <- merge(all_sum_metrics, merged_stats, all=TRUE, by = "las_name")
stats_plus_micro <- stats_plus_micro[,-c(2)]
#FINAL EXPORT
#write.csv(stats_plus_micro, "FILEPATH/temp_vs_metric/stats_plus_micro.csv")


###########################################################
#######HOURLY AIR (!) TEMPERATURES WITHOUT SUMMARY#########
###########################################################
setwd("E://")
plot_name = "las_1"
header <- c("egal", "date", "egal2", "t_soil", "t_surface", "t_air", "egal3", "egal4", "egal5")
# CHANGE FILE NAME
microclimate <- read.csv(paste("FILEPATH/Microclimate/Agroecology_data/",plot_name,"/data_94193801_0.csv", sep=""),
                              header = FALSE, 
                              col.names =  header, 
                              sep = ";")

# DEFINE FUNCTION TO SUMMARIZE HOURLY TEMPERATURE OUT OF 15 MINS INTERVALS
mean_hourly <- function(microclimate, plot_name) {
  microclimate$time <- strptime(microclimate$date, "%Y.%m.%d %H:%M")
  microclimate$date <- as.POSIXct(microclimate$time)
  aggr_microclimate <- aggregate(list(t_air = microclimate$t_air), 
                    list(hourofday = cut(microclimate$date, "1 hour")), 
                    mean)
  aggr_microclimate$plot <- plot_name
  return(aggr_microclimate)
}

# NESTED FUNCTION TO APPLY WHOLE WORKFLOW, NEEDS PRIOR FUNCTION TO WORK
apply_workflow <- function(microclimate){
  microclimate <- microclimate[,c("date", "t_soil", "t_surface", "t_air")]
  hourly_microclimate <- mean_hourly(microclimate, plot_name)
  write.csv(hourly_microclimate, 
            paste("E:/Microclimate/hourly_summary/", plot_name, "_hourly_summary.csv", sep = ""))
}

# APPLY WORKFLOW FUNCTION
apply_workflow(microclimate)

# LIST ALL SUMMARY METRIC FILES
my_files <- list.files("FILEPATH/hourly_summary", pattern = "\\.csv$")
# CHANGE WD FOR LAPPLY
setwd("FILEPATH/hourly_summary")
# READ ALL FILES
sum_metric_list <- lapply(my_files, read.csv)
# COMBINE FILES
all_sum_metrics <- bind_rows(sum_metric_list)
all_sum_metrics <- all_sum_metrics[,-1]
write.csv(all_sum_metrics, "FILEPATH/hourly_summary/all_plots_hourly_summary.csv")


#################################################################################
#######HOURLY TEMPERATURES WITHOUT SUMMARY (ALL TEMPERATURES NOT JUST AIR########
#################################################################################
setwd("FILEPATH")
plot_name = "las_17"
# header <- c("index", "date", "t_soil", "t_surface", "t_air", "time", "hour")
microclimate <- read.csv(paste("FILEPATH/Agroecology_data/",plot_name, "/", plot_name, "_microclimate.csv", sep=""),
                         header = T, 
                         sep = ",")

mean_hourly <- function(microclimate, plot_name) {
  microclimate$time <- as.POSIXct(microclimate$time)
  aggr_microclimate <- aggregate(list(t_soil = microclimate$t_soil,
                                      t_surface = microclimate$t_surface,
                                      t_air = microclimate$t_air), 
                                 list(hourofday = cut(microclimate$time, "1 hour")), 
                                 mean)
  aggr_microclimate$plot <- plot_name
  return(aggr_microclimate)
}


apply_workflow <- function(microclimate){
  microclimate <- microclimate[,c("time", "t_soil", "t_surface", "t_air")]
  hourly_microclimate <- mean_hourly(microclimate, plot_name)
  write.csv(hourly_microclimate, 
            paste("FILEPATH/hourly_summary_all_temps/", plot_name, "_hourly_summary_all_temps.csv", sep = ""),
            row.names = F)
}

apply_workflow(microclimate)

### NOW WE MERGE THEM ALL TO A SINGLE FILE
# LIST ALL SUMMARY METRIC FILES
my_files <- list.files("FILEPATH/hourly_summary_all_temps", pattern = "\\.csv$")
# CHANGE WD FOR LAPPLY
setwd("FILEPATH/hourly_summary_all_temps")
# READ ALL FILES
sum_metric_list <- lapply(my_files, read.csv)
# COMBINE FILES
all_sum_metrics <- bind_rows(sum_metric_list)
all_sum_metrics <- all_sum_metrics[,-1]
write.csv(all_sum_metrics, "FILEPATH/hourly_summary_all_temps/all_plots_hourly_summary_all_temps.csv", row.names = F)
