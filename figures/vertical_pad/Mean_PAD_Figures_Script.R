# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
require(ggthemes)
require(ggplot2)
require(ggimage) 
require(ggtext)
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
# NOT ALL OF THESE ARE TECHNICALLY REQUIRED 

#LOAD CANOPY COVER VALUES
cc_values <- read.csv("FILEPATH/cc_values.csv")
cc_values <- cc_values[,-c(2)]
cc_values$las_name <- cc_values$ID
cc_values$cc_percent <- cc_values$CC....
cc_values$gps_elevation <- cc_values$Elev...m.
cc_values <- cc_values[,-c(2:4)]

# PROVIDE NAME OF THE SITE THAT SHOULD BE PLOTTED AND READ IN
# THE ACCORDING DATASET
las_name = "las_17"
pad.stat <- read.csv(paste("FILEPATH:/Summary_Metrics_DF/", las_name, "_pad_stats.csv", sep = ""))
stats_plus_micro <- read.csv("FILEPATH:/figures/temp_vs_metric/stats_plus_micro_v2.csv")
stats_plus_micro <- stats_plus_micro[,-c(24, 27)]
stats_plus_micro <- left_join(stats_plus_micro, cc_values, by=c('las_name'='las_name'))

# pad.stat <- pad.stat[-c(4:5),]
# trim row
# pad.stat <- pad.stat[-c(9:16),]
# 
# # export trimmed FOR OVERVIEW FIGURE
# write.csv(pad.stat, paste("D:/figures/Mean_PAD_overview/trimmed/", plot_name, "_pad_stats.csv", sep = ""))
# img <- readPNG("D:/figures/canopy_res.png")
# img2 <- readPNG(paste("D:/figures/height_above_ground/outside/edited/transparent/", las_name, "_height_outside.png", sep = ""))

# PROVIDE A BACKGROUND FIGURE IN PNG FORMAT, 
# IN THIS CASE: A SCREENSHOT OF THE POINT CLOUD
img<- readPNG(paste("FILEPATH/", las_name, "_height_inside.png", sep = ""))

# CREATE THE PLOT USING MEDIAN PAD (MED)
p <- ggplot(data = pad.stat, aes(x=med, y=gdist_equal)) +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_ribbon(aes(xmin = med - CI, xmax = med + CI, 
                  fill = "Confidence interval (95%)"), alpha = 0.5)+
  geom_ribbon(aes(xmin = med-se, xmax = med+se, 
                  fill = "Standard error"), alpha = 0.8)+
  geom_path(size = 0.7)+
  geom_point(size = 2)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=11),
        plot.subtitle = element_text(size = 14, face="bold"),
        axis.text = element_text(face="bold"),
        axis.title.x = element_markdown(size = 13))+
  scale_x_continuous( 
                      limits=c(-0.5,1.2), 
                      breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous( "", 
                      limits=c(1,30),
                      breaks = seq(0, 30, 2))+
  # SIMPLE HTML CAN BE PROCESSED BY LABS BECAUSE OF ELEMENT_MARKDOWN IN THE THEME
  # DEPENDENCY IS GGTEXT
  labs(x = "Median PAD (m<sup>2</sup> m<sup>-3</sup> )",
        subtitle = paste(unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"Site"]),
                        " (",las_name, ")",
                        ": ", 
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"cc_percent"]), 
                        "% CC, ",
                        unique(stats_plus_micro[stats_plus_micro$las_name == las_name,"gps_elevation"]),
                        " m a.s.l.",
                        sep = ""), size = 15,
                      fill = "")+
  # THIS LIMITS THE PLOT VIEW ON THE DESIRED COORDINATES AND IS A HOTFIX
  # TO PREVENT GGPLOT's RIBBON CUTOFF IF RIBBON BORDERS EXCEED THE AXIS
  # LIMITS
  coord_cartesian(ylim = c(1,30), xlim = c(0,0.65))+ 
  scale_fill_manual(values=c("#519DDB", "#E2CD5A"))
p

# ANNOTATE AS DESIRED
annotate_figure(
  p,
  # top = text_grob("Average vertical distribution of plant surfaces (PAD)",
  #                 color = "black", face = "bold", size = 14),
  #bottom = text_grob("Mean PAD (m²m-³)", color = "black", size = 12, vjust = -0.2),
  left = text_grob("Height above ground (m)",
                   color = "black", rot = 90, vjust = 2, size = 13),
  right = text_grob("Background represents relative height difference and is not up to scale.",
                    size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

# SAVE AS PDF AND PNG
ggsave(paste("FILEPATH", las_name, "_pda_gg.pdf", sep = ""),
       width = 13,
       height = 20,
       units = "cm")
ggsave(paste("FILEPATH", las_name, "_pda_gg.png", sep = ""),
       width = 13,
       height = 20,
       units = "cm")

