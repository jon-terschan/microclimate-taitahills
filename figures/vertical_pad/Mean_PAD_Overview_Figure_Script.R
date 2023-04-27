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
require(ggtext)
# NOT ALL OF THESE ARE TECHNICALLY REQUIRED 

# LIST ALL PAD DATA FILES (MUST BE IN THE SAME FOLDER) 
filenames <- list.files("FILEPATH", full.names = TRUE)
# READ INTO A LIST
list_data <- lapply(filenames, read.csv)
# NAME THEM AS PER YOUR CHOICE
names(list_data) <- paste('pad.stat', seq_along(filenames), sep = '_')
# CREATE OBJECTS INTO GLOBAL ENVIRONMENT
list2env(list_data, .GlobalEnv)
# BIND THEM INTO A COMMON DATA FRAME
total.pad.stat <- rbind(pad.stat_1, pad.stat_2, pad.stat_3, pad.stat_4,
                        pad.stat_5, pad.stat_6, pad.stat_7, pad.stat_8,
                        pad.stat_9, pad.stat_10, pad.stat_11, pad.stat_12,
                        pad.stat_13, pad.stat_14, pad.stat_15, pad.stat_16,
                        pad.stat_17)
# LOAD SITE DESCRIPTION (MANUALLY CREATED .CSV TABLE)
veg_type <- read.csv("FILEPATH/description_kenya_plots_v3.csv", sep = ";")
# ADAPT TABLE SO IT CAN BE MERGED WITH THE TOTAL PAD STAT DF
veg_type <- rename(veg_type, plot_name = iris_name)
total.pad.stat <- merge(total.pad.stat, veg_type, by = "plot_name")
# IMPORT CANOPY COVER CLASSES DATA AND ADD IT TO THE DATAFRAME (DEPRECATED?)
cc_classes <- read.csv("FILEPATH/cc_classes.csv", sep = ";")
total.pad.stat <- merge(total.pad.stat, cc_classes, by = "plot_name")

# TEST AND MAX AIR ARE FROM THE CORRMAT.R FILE AND NOT PART OF THIS DOCUMENT
total.pad.stat <- merge(total.pad.stat, test, by = "las_name")
total.pad.stat <- merge(total.pad.stat, max_air, by = "las_name")

# library(dplyr)
# total.pad.stat2 <- total.pad.stat %>%
#   mutate(plot_name = recode(plot_name, "iris-1" = "las_1",
#                             "iris-2" = "las_2",
#                             "iris-3" = "las_3",
#                             "iris-4" = "las_4",
#                             "iris-5" = "las_5",
#                             "iris-6" = "las_6",
#                             "iris-7" = "las_7",
#                             "iris-8" = "las_8",
#                             "iris-9" = "las_9",
#                             "iris-10" = "las_10",
#                             "iris-11" = "las_11",
#                             "iris-12" = "las_12",
#                             "iris-13" = "las_13",
#                             "iris-14" = "las_14",
#                             "iris-15" = "las_15",
#                             "iris-16" = "las_16",
#                             "iris-17" = "las_17"))
# 
# total.pad.stat2 <- total.pad.stat2 %>% 
#   rename(
#      plot_name = las_name,
#   )

# DEPRECATED
# scale <- c("#98babd","#6a91ba","#4169ae","#00429d","#ffd778")

# DEPRECATED
# plot_name = "las_17"
# import summary stats
# pad.stat <- read.csv(paste("Summary_Metrics_DF/", plot_name, "_pad_stats.csv", sep = ""))

# READ IN BACKGROUND IMAGE AS PNG
img<- readPNG(paste("D:/figures/canopy_res_overview.png", sep = ""))

# CREATE THE FIRST PLOT
t1 <- ggplot(data = total.pad.stat, aes(x=med, y=gdist_equal, group = las_name, color = t_air_range))+
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_path(size = 0.8)+
  scale_x_continuous(limits=c(0, 0.9),
                     expand = c(0, 0), 
                     breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous("Height above Ground (m)", 
                     limits=c(1,30),
                     expand = c(0, 0),
                     breaks = seq(0, 30, 2))+
  theme_light()+
  theme(legend.position = "none",
        legend.justification = "left",
        axis.title.y=element_blank(),
        axis.title.x = element_markdown(),
        axis.text = element_text(face="bold"))+
  labs(title = "Maximum Air Temperature Range",
       caption = "",
       fill = "",
       x = "Median PAD (m<sup>2</sup> m<sup>-3</sup>)")+
  scale_color_gradient(low = "purple", high = "yellow", name = "",limits = c(0,37)) 

# DEPRECATED
# t1 + guides(color = guide_legend(override.aes = list(size = 15)))
# scale2 <- scale

# CREATE SECOND PLOT
t2 <- ggplot(data = total.pad.stat, aes(x=med, y=gdist_equal, group = plot_name, color = name))+
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_path(size = 0.8)+
  scale_x_continuous( 
                      expand = c(0, 0),
                      limits=c(0,0.9), 
                      breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous( "Height above Ground (m)", 
                      limits=c(1,30),
                      expand = c(0, 0),
                      breaks = seq(0, 30, 2))+
  theme_light()+
  theme(legend.position = "bottom",
        legend.justification = "center",
        axis.title.y=element_blank(),
        axis.title.x = element_markdown(),
        axis.text = element_text(face="bold"))+
  labs(title = "Median Air Temperature Range",
       caption = "",
       fill = "",
       x = "Median PAD (m<sup>2</sup> m<sup>-3</sup>)")+
  scale_color_gradient(low = "purple", high = "yellow", name = "Temperature range (°C)", limits = c(0,40)) 
# EXTRACT COMMON LEGEND
legend <- get_legend(t2)

# OVERWRITE SECOND PLOT WITH SAME PLOT (-LEGEND)
t2 <- ggplot(data = total.pad.stat, aes(x=med, y=gdist_equal, group = plot_name, color = name))+
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_path(size = 0.8)+
  scale_x_continuous( 
                      expand = c(0, 0),
                      limits=c(0,0.9), 
                      breaks = seq(0, 1.2, 0.2))+
  scale_y_continuous( "Height above Ground (m)", 
                      limits=c(1,30),
                      expand = c(0, 0),
                      breaks = seq(0, 30, 2))+
  theme_light()+
  theme(legend.position = "none",
        legend.justification = "right",
        axis.title.y=element_blank(),
        axis.title.x = element_markdown(),
        axis.text = element_text(face="bold"))+
  labs(title = "Median Air Temperature Range",
       caption = "",
       fill = "",
       x = "Median PAD (m<sup>2</sup> m<sup>-3</sup>)")+
  scale_color_gradient(low = "purple", high = "yellow", name = "Range (°C)", limits = c(0,37)) 


# ARRANGE INDIVIDUAL PLOTS WITH PARSER AND COMMON LEGEND
figure <- ggarrange(t2, t1,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1 ,
                    legend = "bottom",
                    common.legend = T,
                    legend.grob = legend
)

figure
# ADD ANNOTATION
annotate_figure(
  figure,
  #top = text_grob("Vertical PAD Distribution and Canopy Cover (CC)",
                  #color = "black", face = "bold", size = 14),
  # bottom = text_grob("Median PAD (m²m-³)", color = "black", size = 12),
  left = text_grob("Height above ground (m)",
                   color = "black", rot = 90)
  # right = text_grob("Background represents relative height difference and is not up to scale.",
  #                   size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

# EXPORT PLOT AS PDF OR PNG
ggsave(paste("D:/figures/Mean_PAD_overview/PAD_med_range_overview.pdf", sep = ""),
       width = 18,
       height = 19,
       units = "cm")
ggsave(paste("D:/figures/Mean_PAD_overview/PAD_med_range_overview.png", sep = ""),
       width = 18,
       height = 19,
       units = "cm")
