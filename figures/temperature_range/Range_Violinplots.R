# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
library(Hmisc)
library(corrplot)
library(dplyr)
library(ggpubr)
library(cowplot)
library(dplyr)
library(timetk)
library(forcats)
require(ggimage) 
require(png)

# READ IN LARGE SUMMARY DATAFRAME
model_df <- read.csv("FILEPATH/model_df_2_1.csv")
model_df$hourofday <- as.POSIXct(model_df$hourofday)

# SUMMARIZE DAILY MIN AND MAX TEMPERATURES
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

# CALCULATE DTR AND ADD STRUCTURAL METRICS TO DTR DATAFRAME
daily_range$t_soil_range = (daily_range$t_soil_max - daily_range$t_soil_min)
daily_range$t_air_range = (daily_range$t_air_max - daily_range$t_air_min)
model_df <- model_df[,-c(2:7)]
unique_model_df <- unique(model_df)
unique_model_df <- unique_model_df[-14,]
daily_range  <- left_join(daily_range, unique_model_df, by = "las_name")

# LOAD CC VALUES AND ADD THEM TO DF, REMOVE UNNECESSARY STUFF FIRST
cc_values <- read.csv("D:/modeling/cc_values.csv")
cc_values <- cc_values[,-c(2)]
cc_values$las_name <- cc_values$ID
cc_values$cc_percent <- cc_values$CC....
cc_values$gps_elevation <- cc_values$Elev...m.
cc_values <- cc_values[,-c(2:4)]
daily_range  <- left_join(daily_range, cc_values, by = "las_name")

# AIR TEMPERATURE RANGE
air_range <- ggplot()+
  geom_violin(daily_range, 
              mapping = aes(x = t_air_range, y=factor(fct_reorder(Site, ENL2_mean)), 
                            fill = cc_percent.y), alpha = 0.6)+
  geom_boxplot(daily_range,
               mapping = aes(x = t_air_range, y=factor(fct_reorder(Site, ENL2_mean)), 
                             fill = cc_percent.y), alpha = 0.6,
               width = 0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        plot.margin=unit(c(1,1,1,1), "pt"),
        legend.position = "top",
        legend.direction='vertical')+
  labs(x = "Daily temperature range (°C)", y = "", title = "Air Temperature")+
  xlim(0,35)+
  scale_fill_gradient("CC (%)",
    low = "#E5AA09",
    high = "#0C7BDC",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  )
# GET LEGEND
legend <- get_legend(air_range)

# AIR TEMPERATURE RANGE AGAIN BUT WITHOUT LEGEND FOR FINAL FIGURE
air_range <- ggplot()+
  geom_violin(daily_range, 
              mapping = aes(x = t_air_range, y=factor(fct_reorder(Site, ENL2_mean)), 
                            fill = cc_percent.y), alpha = 0.6)+
  geom_boxplot(daily_range,
               mapping = aes(x = t_air_range, y=factor(fct_reorder(Site, ENL2_mean)), 
                             fill = cc_percent.y), alpha = 0.6,
               width = 0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.subtitle =  element_text(size = 14),
        plot.margin=unit(c(1,1,1,1), "pt"),
        legend.position = "none",
        legend.direction='horizontal')+
  labs(x = "", y = "", title = "Air Temperature")+
  xlim(0,35)+
  scale_fill_gradient("CC%",
                      low = "#E5AA09",
                      high = "#0C7BDC",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill"
  )

# SOIL TMEPERATURE RANGE
sor_range <- ggplot()+
  geom_violin(daily_range, 
              mapping = aes(x = t_soil_range, y=factor(fct_reorder(Site, cc_percent.y)), 
                            fill = cc_percent.y), alpha = 0.6)+
  # geom_boxplot(daily_range,
  #              mapping = aes(x = t_soil_range, y=factor(fct_reorder(Site, t_air_range)), 
  #                            fill = veg_class), alpha = 0.6,
  #              width = 0.1)+
  theme_bw()+
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 10),
        axis.text.y = element_blank(),
        plot.subtitle =  element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        plot.margin=unit(c(1,70,1,10), "pt"),
        legend.position = "none",
        legend.direction='horizontal')+
  labs(x = "", y = "", title = "Soil Temperature")+
  xlim(0,35)+
  scale_fill_gradient("CC%",
                      low = "#E5AA09",
                      high = "#0C7BDC",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill"
  )

# THIS IS THE MOST MIND BOGGLING THING BUT I COULDNT FIND A BETTER SOLUTION
# TO REMOVE THE WHITE PADDING GG ARRANGES PLACES INSTEAD OF THE Y AXIS LABEL
# FOR THE RIGHT COLUMN
# I MANUALLY REMOVED THE PADDING WITHIN THE PLOTS, USED PLOT GRID
# AND THEN ADDED MORE PADDING ON THE RIGHT SIDE TO SORT OF ALIGN THEM
#labels=c("A", "B")
fig1 <- plot_grid(air_range, sor_range, ncol = 2, nrow = 1,
          align = "h") + theme(plot.margin = margin(0,-2,0,0, "cm")) 

fig2 <- ggarrange(fig1, 
                  nrow = 1,
                  legend.grob = legend,
                  legend = "right"
                  
)

fig2
# #  scale_fill_manual("Vegetation Class", values = c("#EE6677", "#AA3377", "#66CCEE", "#CCBB44"),
# aesthetics = "fill",
# breaks = waiver(),
# na.value = "grey50")+

# ANNOTATE FIGURE
fig1
annotate_figure(
   fig2,
#   #top = text_grob("", color = "black", face = "bold", size = 14),
 bottom = text_grob("DTR - Daily temperature range (°C)", color = "black", size = 14)
#   #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
#   #size = 8, rot = 270, face = "italic")
#   # fig.lab = "Figure 1", fig.lab.face = "bold"
 )

# SAVE FIGURE
ggsave("FILEPATH/yearly_range.png",
       width = 20,
       height = 22,
       units = "cm")
