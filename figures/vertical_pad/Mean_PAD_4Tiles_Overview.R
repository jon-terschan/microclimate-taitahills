# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
require(ggthemes)
require(ggplot2)
require(plotrix)
require(plotfunctions)
require(raster)
require(fANCOVA)
require(rgeos)
require(dplyr)
require(ggpubr)
require(jpeg)
require(cowplot)
require(magick)
require(png)
require(ggimage) 
require(gridtext) 
# NOT ALL OF THESE ARE TECHNICALLY REQUIRED 
# THIS SCRIPT GENERATES THE VERTICAL PAD DISTRIBUTION FIGURE WITH 4 TILES
# AND SERVES AS A TEMPLATE TO CREATE PAD COMPARISON FIGURES

#LOAD CANOPY COVER VALUES
cc_values <- read.csv("FILEPATH/cc_values.csv")
cc_values <- cc_values[,-c(2)]
cc_values$las_name <- cc_values$ID
cc_values$cc_percent <- cc_values$CC....
cc_values$gps_elevation <- cc_values$Elev...m.
cc_values <- cc_values[,-c(2:4)]

# E.G. A SITE WITH LOW CANOPY COVER
las_name = "las_9"
pad.stat <- read.csv(paste("D:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
stats_plus_micro <- read.csv("D:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))

img<- readPNG(paste("D:/figures/height_above_ground/inside/edited/transparent/", las_name, "_height_inside.png", sep = ""))

t1 <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
                  fill = "Confidence interval (95%)"), alpha = 0.5)+
  geom_ribbon(aes(xmin = med-se, xmax = med+se, 
                  fill = "Standard error"), alpha = 0.8)+
  geom_path(size = 0.7)+
  geom_point(size = 2)+
  scale_x_continuous(limits=c(-0.5,0.7), 
                      breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous(limits=c(1,30),
                      breaks = seq(0, 30, 2))+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size=11),
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(face="bold"))+
  labs(subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
                        ": ", 
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
                        "% CC, ",
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
                        " m a.s.l.",
                        sep = ""),
       fill = "")+
  coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
  scale_fill_manual(values=c("#519DDB", "#E2CD5A"))

# E.G. A SITE WITH LOW-MID CANOPY COVER
las_name = "las_11"
pad.stat <- read.csv(paste("D:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
stats_plus_micro <- read.csv("D:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))

img<- readPNG(paste("D:/figures/height_above_ground/inside/edited/transparent/", las_name, "_height_inside.png", sep = ""))

t2 <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
                  fill = "CI95"), alpha = 0.5)+
  geom_ribbon(aes(xmin = med-se, xmax = med+se, 
                  fill = "Std. Err."), alpha = 0.8)+
  geom_path(size = 0.7)+
  geom_point(size = 2)+
  scale_x_continuous(limits=c(-0.5,0.7), 
                      breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous(limits=c(1,30),
                      breaks = seq(0, 30, 2))+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 12),
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(face="bold"))+
  labs(subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
                        ": ", 
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
                        "% CC, ",
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
                        " m a.s.l.",
                        sep = ""),
       fill = "")+
  coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
  scale_fill_manual(values=c("#519DDB", "#E2CD5A"))
 
# E.G. A SITE WITH MID CANOPY COVER
# las_name = "las_5"
# pad.stat <- read.csv(paste("D:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
# stats_plus_micro <- read.csv("D:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
# stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
# stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))
# 
# img<- readPNG(paste("D:/figures/height_above_ground/inside/edited/transparent/", las_name, "_height_inside.png", sep = ""))
# 
# t3 <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
#   annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
#                   fill = "CI95"), alpha = 0.5)+
#   geom_ribbon(aes(xmin = med-se, xmax = med+se, 
#                   fill = "Std. Err."), alpha = 0.8)+
#   geom_path(size = 0.7)+
#   geom_point(size = 2)+
#   scale_x_continuous(limits=c(-0.5,0.7), 
#                       breaks = seq(0, 1.2, 0.2))+
#   scale_y_continuous(limits=c(1,30),
#                       breaks = seq(0, 30, 2))+
#   theme_bw()+
#   theme(legend.position = "bottom",
#         plot.subtitle = element_text(size = 9),
#         legend.title=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text = element_text(face="bold"))+
#   labs(subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
#                         ": ", 
#                         unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
#                         "% CC, ",
#                         unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
#                         " m a.s.l.",
#                         sep = ""),
#        fill = "")+
#   coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
#   scale_fill_manual(values=c("#519DDB", "#E2CD5A"))

# E.G. A SITE WITH MID-TO HIGH CANOPY COVER
las_name = "las_5"
pad.stat <- read.csv(paste("D:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
stats_plus_micro <- read.csv("D:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))

img<- readPNG(paste("D:/figures/height_above_ground/inside/edited/transparent/", las_name, "_height_inside.png", sep = ""))

b1 <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
                  fill = "CI95"), alpha = 0.5)+
  geom_ribbon(aes(xmin = med-se, xmax = med+se, 
                  fill = "Std. Err."), alpha = 0.8)+
  geom_path(size = 0.7)+
  geom_point(size = 2)+
  scale_x_continuous(limits=c(-0.5,0.7), 
                     breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous(limits=c(1,30),
                     breaks = seq(0, 30, 2))+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 12),
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(face="bold"))+
  labs(subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
                        ": ", 
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
                        "% CC, ",
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
                        " m a.s.l.",
                        sep = ""),
       fill = "")+
  coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
  scale_fill_manual(values=c("#519DDB", "#E2CD5A"))

# ANOTHER SITE
las_name = "las_6"
pad.stat <- read.csv(paste("D:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
stats_plus_micro <- read.csv("D:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))

img<- readPNG(paste("D:/figures/height_above_ground/inside/edited/transparent/", las_name, "_height_inside.png", sep = ""))

b2 <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
                  fill = "CI95"), alpha = 0.5)+
  geom_ribbon(aes(xmin = med-se, xmax = med+se, 
                  fill = "Std. Err."), alpha = 0.8)+
  geom_path(size = 0.7)+
  geom_point(size = 2)+
  scale_x_continuous(limits=c(-0.5,0.7), 
                     breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous(limits=c(1,30),
                     breaks = seq(0, 30, 2))+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 12),
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(face="bold"))+
  labs(subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
                        ": ", 
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
                        "% CC, ",
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
                        " m a.s.l.",
                        sep = ""),
       fill = "")+
  coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
  scale_fill_manual(values=c("#519DDB", "#E2CD5A"))

# A SIXTH SITE
# las_name = "las_5"
# pad.stat <- read.csv(paste("D:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
# stats_plus_micro <- read.csv("D:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
# stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
# stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))
# 
# img<- readPNG(paste("D:/figures/height_above_ground/inside/edited/transparent/", las_name, "_height_inside.png", sep = ""))
# 
# b3 <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
#   annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
#                   fill = "CI95"), alpha = 0.5)+
#   geom_ribbon(aes(xmin = med-se, xmax = med+se, 
#                   fill = "Std. Err."), alpha = 0.8)+
#   geom_path(size = 0.7)+
#   geom_point(size = 2)+
#   scale_x_continuous(limits=c(-0.5,0.7), 
#                       breaks = seq(0, 1.2, 0.2))+
#   scale_y_continuous(limits=c(1,30),
#                       breaks = seq(0, 30, 2))+
#   theme_bw()+
#   theme(legend.position = "bottom",
#         plot.subtitle = element_text(size = 9),
#         legend.title=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text = element_text(face="bold"))+
#   labs(subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
#                         ": ", 
#                         unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
#                         "% CC, ",
#                         unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
#                         " m a.s.l.",
#                         sep = ""),
#        fill = "")+
#   coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
#   scale_fill_manual(values=c("#519DDB", "#E2CD5A"))
# b3

#### RETRIEVE COMMON LEGEND FROM FIGURE 1
legend <- get_legend(t1)

## ARRANGE INDIVIDUAL PLOTS WITH PARSER
# I REMOVED T3 AND B3 BECAUSE THE FLOAT IS BECOMING TOO LARGE FOR THE DRAFT
# HOWEVER YOU CAN ADD INFINITE SUBFIGURES USING GGARRANGE
figure <- ggarrange(t1, t2, b1, b2,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,
                    legend = "bottom",
                    common.legend = T,
                    legend.grob = legend
)

figure
# ADD ANNOTATION 
annotate_figure(
  figure,
  # top = text_grob("Average vertical distribution of plant surfaces (PAD)",
  #                 color = "black", face = "bold", size = 14),
  bottom = richtext_grob("<span style='font-size:12pt;
                          color:black'>Median PAD (m<sup>2</sup> m<sup>-3</sup> )
                          </span>
                         
                         "),
  left = text_grob("Height above ground (m)",
                   color = "black", rot = 90, size = 12),
  right = text_grob("Background represents relative height difference and is not up to scale.",
                    size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

# GET WORKING DIRECTORY
getwd()
# CHANGE WORKING DIRECTORY TO DESIRED TARGET FOLDER USING SET WD
# SAVE AS PDF AND PNG
ggsave(paste("PAD_med_overview_CI952.pdf", sep = ""),
       width = 18,
       height = 25,
       units = "cm")
ggsave(paste("PAD_med_overview_CI952.png", sep = ""),
       width = 18,
       height = 25,
       units = "cm")
dev.off()

