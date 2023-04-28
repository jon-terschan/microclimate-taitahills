# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
library(Hmisc)
library(corrplot)
library(dplyr)
library(ggpubr)
library(cowplot)
library(dplyr)
library(timetk)
require(scales)

# SET SYSTEM LOCALE TO ENGLISH TO ENSURE ENGLISH AXIS LABELS
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL","English")

# LOAD DATAFRAME WITH ALL DATA AND MICROCLIMATE MEASUREMENTS
model_df <- read.csv("FILEPATH/model_df_2_1.csv")
model_df$hourofday <- as.POSIXct(model_df$hourofday)


# SUMMARIZE FOR EACH DAY
daily_range <- model_df %>%
  group_by(las_name) %>%
  summarise_by_time(
    .date_var = hourofday,
    .by       = "day", 
    # Summarization
    t_soil_min  = min(t_soil),
    t_soil_max  = max(t_soil),
    #t_soil_range = range(t_soil),
    #t_surface_range = range(t_surface),
    t_air_min  = min(t_air),
    t_air_max  = max(t_air),
    #t_air_range = range(t_air)
  )

# CALCULATE DAILY TEMPERATURE RANGES
daily_range$t_soil_range = (daily_range$t_soil_max - daily_range$t_soil_min)
daily_range$t_air_range = (daily_range$t_air_max - daily_range$t_air_min)

# SUMMARIZE FOR EACH MONTH
monthly_range <- daily_range %>%
  group_by(las_name) %>%
  summarise_by_time(
    .date_var = hourofday,
    .by       = "month", 
    # Summarization
    air_range_mean  = mean(t_air_range),
    air_range_min = min(t_air_range),
    air_range_max = max(t_air_range),
    #t_soil_range = range(t_soil),
    #t_surface_range = range(t_surface),
    sor_range_mean  = mean(t_soil_range),
    sor_range_min = min(t_soil_range),
    sor_range_max = max(t_soil_range),
    #t_air_range = range(t_air)
  )

model_df <- model_df[,-c(2:7)]
# MELT DATAFRAME UNIQUE COLUMNS TO GET THE STRUCTURAL METRICS
unique_model_df <- unique(model_df)
unique_model_df <- unique_model_df[-14,]

# JOIN WITH MONTHLY RANGE TO GET A DF WITH MONTHLY RANGES + STRUCTURAL METRICS
monthly_range_join <- left_join(monthly_range, unique_model_df, by = "las_name")
monthly_range_join$hourofday <- as.Date(monthly_range_join$hourofday)
monthly_range_join$FHD2_mean <- as.numeric(monthly_range_join$FHD2_mean)

# MEAN MONTHLY RANGE + FHD
air_mean <- ggplot(monthly_range_join, aes(x=hourofday, y=air_range_mean, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("Mean FHD",
    low = "purple",
    high = "yellow",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1),
        axis.title.y=element_text(size=9))+
  labs(x = "", y = "Mean monthly range (°C)", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)
# GET LEGEND FOR GGARRANGE
legend <- get_legend(air_mean)

# MEAN MONTHLY RANGE + FHD, THIS TIME WITHOUT LEGEND SO ITS NOT DOUBLED IN THE
# FINAL PLOT
air_mean <- ggplot(monthly_range_join, aes(x=hourofday, y=air_range_mean, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("Mean FHD",
                        low = "purple",
                        high = "yellow",
                        space = "Lab",
                        na.value = "grey50",
                        guide = "colourbar",
                        aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1),
        axis.title.y=element_text(size=9))+
  labs(x = "", y = "Mean monthly range (°C)", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)

# MAX MONTHLY RANGE + FHD
air_max <- ggplot(monthly_range_join, aes(x=hourofday, y=air_range_max, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("",
                        low = "purple",
                        high = "yellow",
                        space = "Lab",
                        na.value = "grey50",
                        guide = "colourbar",
                        aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1),
        axis.title.y=element_text(size=9))+
  labs(x = "", y = "Max. monthly range (°C)", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)

# MIN MONTHLY RANGE + FHD
air_min <- ggplot(monthly_range_join, aes(x=hourofday, y=air_range_min, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("",
                        low = "purple",
                        high = "yellow",
                        space = "Lab",
                        na.value = "grey50",
                        guide = "colourbar",
                        aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1),
        axis.title.y=element_text(size=9))+
  labs(title = "Air Temperature", x = "", y = "Min. monthly range (°C)", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)

# GGARRANGE LEFT COLUMN OF FINAL PLOT
fig1 <- ggarrange(air_min, air_mean, air_max,
                  labels = c("A", "B", "C"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)

# SOIL MEAN MONTHLY RANGE + FHD
sor_mean <- ggplot(monthly_range_join, aes(x=hourofday, y=sor_range_mean, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("Mean FHD",
                        low = "purple",
                        high = "yellow",
                        space = "Lab",
                        na.value = "grey50",
                        guide = "colourbar",
                        aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1))+
  labs(x = "", y = "", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)


# SOIL MAX MONTHLY RANGE + FHD
sor_max <- ggplot(monthly_range_join, aes(x=hourofday, y=sor_range_max, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("",
                        low = "purple",
                        high = "yellow",
                        space = "Lab",
                        na.value = "grey50",
                        guide = "colourbar",
                        aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1),
        axis.title.y=element_text(size=14))+
  labs(x = "", y = "", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)

# SOIL MIN MONTHLY RANGE + FHD
sor_min <- ggplot(monthly_range_join, aes(x=hourofday, y=sor_range_min, color = FHD_mean, group = factor(las_name)))+
  geom_path()+
  geom_point(shape = 20, size = 2)+
  theme_bw()+
  scale_colour_gradient("",
                        low = "purple",
                        high = "yellow",
                        space = "Lab",
                        na.value = "grey50",
                        guide = "colourbar",
                        aesthetics = "colour"
  )+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 1))+
  labs(title = "Soil Temperature", x = "", y = "", fill = "Mean FHD")+
  geom_rect(data=monthly_range_join[1,], 
            aes(xmin=as.Date("2019-05-31"), xmax=as.Date("2019-07-10"), ymin=-Inf,ymax=Inf), 
            alpha=0.3, 
            fill="grey",
            color = NA)+
  annotate("text", x=as.Date("2019-07-15"), y=31, size = 4, label="(*)", alpha = 0.3,
           hjust = 2.5, vjust = -2)+
  scale_x_date(labels = date_format("%b"), 
               breaks = date_breaks("month"), 
               limits = c(as.Date("2019-5-28"), as.Date("2020-02-02")))+
  ylim(0,40)

# GGARRANGE RIGHT COLUMN OF FINAL PLOT
fig2 <- ggarrange(sor_min, sor_mean, sor_max, 
                  labels = c("D", "E", "F"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)

# GGARRANGE FINAL PLOT WITH COMMON LEGEND
figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv",
                    common.legend = T,
                    legend.grob = legend,
                    legend = "bottom"
)
figure

# ANNOTATE 
annotate_figure(
  figure,
  #top = text_grob("", color = "black", face = "bold", size = 14),
  bottom = text_grob("(*) 13.06.2019 - 10.07.2019, study period: Aalto et al. (2022)", color = "black",
                     hjust = 1, x = 1, size = 10)
  #left = text_grob("Daily temperature range (°C)",
  #color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

# SAVE FIGURE
ggsave("FILEPATH/monthly_ranges.pdf",
       width = 20,
       height = 23,
       units = "cm")


