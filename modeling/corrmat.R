#install.packages("XYZ")
require(Hmisc)
require(corrplot)
require(dplyr)
require(ggpubr)
require(cowplot)
require(car)
require(png)
require(ggimage) 
require(tidyverse)
require(caret)
require(leaps)

##################################
##########DATAFRAME CURATION######
##################################
# read model df, this one is deprecated and must be adjusted
model_df <- read.csv("FILEPATH/model_df_2_1.csv")
# model_df$hourofday <- as.POSIXct(model_df$hourofday)
# remove normal temperature reading columns
model_df <- model_df[,-c(2:7)]
# melt down to unique columns
unique_model_df <- unique(model_df)
#get rid of double row introduced by rounding (??)
unique_model_df <- unique_model_df[-14,]
#get rid of gps_elevation and cc_percent (will be added soon)
unique_model_df <- unique_model_df[,-c(8, 11)]

# read in daily range
daily_range <- read.csv("FILEPATH/daily/daily_range.csv")
daily_range$hourofday <- as.Date(daily_range$hourofday)

# read in diurnal/nightly temp offset
offset_day <- read.csv("FILEPATH/Daily_Range/daily/daily_offset_day.csv")
offset_day$hourofday <- as.Date(offset_day$hourofday)
offset_day <- rename(offset_day, 
                     offd_mn = offset_mean,
                     offd_med = offset_median,
                     offd_max = offset_max,
                     offd_min = offset_min)
offset_day <- offset_day[,-c(3,4,5,6,7,8,9,10)]

offset_night <- read.csv("FILEPATH/Daily_Range/daily/daily_offset_night.csv")
offset_night$hourofday <- as.Date(offset_day$hourofday)
offset_night <- rename(offset_night, 
                       offn_mn = offset_mean,
                       offn_med = offset_median,
                       offn_max = offset_max,
                       offn_min = offset_min)
offset_night <- offset_night[,-c(3,4,5,6,7, 8,9,10)]

#OFFSET WITHOUT DIURNAL/NIGHT TEMP SPLIT (DEPRECATED)
#daily_offset <- read.csv("FILEPATH/Daily_Range/daily/daily_offset.csv")
#daily_offset$hourofday <- as.Date(daily_offset$hourofday)
# join range and diurnal offset
results <- left_join(daily_range, offset_day, by=c('las_name'='las_name', 'hourofday'='hourofday'))
# join range, diurnal offset and nightly offset
results <- left_join(results, offset_night, by=c('las_name'='las_name', 'hourofday'='hourofday'))
# remove surface temp (unneccessary)
results <- results[,-c(5, 6, 10)]

# load in CC values and GPS elevation from iris thesis
cc_values <- read.csv("FILEPATH/modeling/cc_values.csv")
cc_values <- cc_values[,-c(1,2)]
cc_values$las_name <- cc_values$ID
cc_values$cc_percent <- cc_values$CC....
cc_values$gps_elevation <- cc_values$Elev...m.
cc_values <- cc_values[,-c(1:3)]
# join CC values with the other values
results_with_cc <- left_join(results, cc_values, by=c('las_name'='las_name'))
# join structural traits with CC values and the rest
results_df <- left_join(results_with_cc, unique_model_df, by=c('las_name'='las_name'))
# write and load
write.csv(results_df, "FILEPATH/Modeling/corrmat/results_df.csv", row.names = F)
results_df <- read.csv("FILEPATH/Modeling/corrmat/results_df.csv")

# LITTLE INTERJECTION TO DETERMINE MIN MAX MED RANGE FOR EACH STUDY SITE
# TO BE USED IN DIFFERENT SCRIPTS (THIS IS USED IN THE 4 TILES PAD FIGURE)
require(dplyr)
min(results_df$t_air_range)
unique(results_df)
max_air <- results_df %>% group_by(las_name) %>% slice_max(n = 1, t_air_range)
max_air <- results_df %>% group_by(las_name) %>% slice_min(n = 1, t_air_range)
test <- results_df %>% group_by(las_name) %>% 
  dplyr::summarise_at(vars(t_air_range), list(name = median))


##################################
##########CORRMAT SOR AND AIR#####
##################################
# remove columns (trim df for cormat)
results_corrmat <- dplyr::select(results_df, 
                          -c("las_name", "hourofday",
                             "t_soil_min","t_soil_max","t_air_min","t_air_max",
                             "offd_med", "offd_max", "offd_min", "offn_med", "offn_max",
                             "offn_min", "offd_mn", "offn_mn", "RH95_max", "RH50_max", "RH25_max",
                             #"t_air_mean", "t_air_med",
                             #"t_ERA5_mean", "t_ERA5_med",
                             "pad_n", "pad_min", "pad_sd", "pad_n",
                             "cc_class", "als_elevation",
                             "slope", "aspect",
                             "veg_type", "veg_class",
                             "RH98_mean", "RH98_med", "RH98_sd", "RH98_min", "RH98_max",
                             "RH75_mean", "RH75_med", "RH75_sd", "RH75_min", "RH75_max",
                             "CR_min", "FHD_min", "pai_min", "RH25_min", "RH50_min", "RH95_min",
                             "FHD2_mean", "FHD2_max", "FHD2_sd",
                             "ENL1_mean", "ENL1_max", "ENL1_sd",
                             "CR_sd", "RH25_sd", "pai_med", "pad_med", "ENL2_sd",
                             "pai_sd", "RH25_med", "RH50_med", "RH50_sd","RH95_med", "FHD_med",
                             "FHD_sd", "RH95_sd", "CR_med"))

# CODE TO CREATE CORRELATION MATRIX
corr_simple <- function(data=df,sig=0.001){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr,
           type = "upper", method = "square", addCoef.col = "black", number.cex = 0.75,
           col=colorRampPalette(c("#A89100","white","#2D6993"))(100),
           is.corr=FALSE, tl.col="black", tl.cex = 1.2, na.label=" ")
return(corr)
}

corr <- corr_simple(results_corrmat)
corr_trim <-subset(corr, Freq > 0.2 | Freq <= -0.2)
corr_trim$Freq <- round(corr_trim$Freq, 3)
corr_trim <- corr_trim[corr_trim$Var1 %in% c("offset_day_mean", "offset_night_mean", "t_soil_range", "t_air_range", "cc_percent", "gps_elevation"),]
rownames(corr_trim) <- 1:nrow(corr_trim)

write.csv(corr_trim, "FILEPATH/Modeling/corrmat/corr_check.csv", row.names = F)

corr_renamed <- rename(results_corrmat, 
                       sor = t_soil_range,
                       air = t_air_range,
                       cc = cc_percent,
                       elev = gps_elevation,
                       pad_mn = pad_mean,
                       pad_mx = pad_max,
                       FHD_mn = FHD_mean,
                       FHD_mx = FHD_max,
                       CR_mn = CR_mean,
                       CR_mx = CR_max,
                       pai_mn = pai_mean,
                       pai_mx = pai_max,
                       RH25_mn = RH25_mean,
                       RH50_mn = RH50_mean,
                       RH95_mn = RH95_mean,
                       ENL2_mn = ENL2_mean,
                       ENL2_mx = ENL2_max
                       )


# Step 1: Call the pdf command to start the plot
pdf(file = "FILEPATH:/Modeling/corrmat/corrmat_spearman_updated.pdf", width = 10  ,
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
corr <- corr_simple(corr_renamed)

# Step 3: Run dev.off() to create the file!
dev.off()

corr <- corr_simple(corr_renamed)
# calculate spearman with base r 
# res <- cor(results_corrmat, method=c("spearman"))
# # round so its readable
# res <- round(res, 2)
# 
# # calculate spearman and p values, check for p values
# res2 <- rcorr(as.matrix(results_corrmat), type = "spearman")
# corrmat <- res2$r
# corrmat_p <-res2$P
# # replace NAs with 0 
# corrmat_p <-replace(corrmat_p,is.na(corrmat_p),0)
# 
# # visualize using corrplot
# corrplot(corrmat, type = "upper", order = "hclust", 
#          method = "square",
#          p.mat = corrmat_p, 
#          sig.level = 0.01,
#          insig = "p-value",
#          col=colorRampPalette(c("#A89100","white","#2D6993"))(100),
#          tl.col = "black", 
#          tl.srt = 45,
#          tl.cex = 0.75)



#########################################
##########CORRMAT OFFSETDAY AND NIGHT#####
##########################################ä#
# remove columns (trim df for cormat)
results_corrmat <- dplyr::select(results_df, 
                                 -c("las_name", "hourofday",
                                    #"t_soil_range", "t_air_range"
                                    "offd_mn", "offn_mn",
                                    "t_soil_min","t_soil_max","t_air_min","t_air_max",
                                    "pad_max", "RH25_max", "RH50_max",
                                    #"t_air_mean", "t_air_med",
                                    #"t_ERA5_mean", "t_ERA5_med",
                                    "pad_n", "pad_min", "pad_sd", "pad_n",
                                    "cc_class", "als_elevation",
                                    "slope", "aspect",
                                    "veg_type", "veg_class",
                                    "RH98_mean", "RH98_med", "RH98_sd", "RH98_min", "RH98_max",
                                    "RH75_mean", "RH75_med", "RH75_sd", "RH75_min", "RH75_max",
                                    "CR_min", "FHD_min", "pai_min", "RH25_min", "RH50_min", "RH95_min",
                                    "FHD2_mean", "FHD2_max", "FHD_sd",
                                    "ENL1_mean", "ENL1_max", "ENL1_sd", "FHD2_sd",
                                    "CR_sd", "RH25_sd", "pai_med", "pad_med", "ENL2_sd",
                                    "pai_sd", "RH25_med", "RH50_med", "RH50_sd","RH95_med", "RH95_max", "FHD_med",
                                    "FHD_sd", "RH95_sd", "CR_med"))

# CODE TO CREATE CORRELATION MATRIX
corr_simple <- function(data=df,sig=0.001){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr,
           type = "upper", method = "square", addCoef.col = "black", number.cex = 0.75,
           col=colorRampPalette(c("#A89100","white","#2D6993"))(100),
           is.corr=FALSE, tl.col="black", tl.cex = 1.2, na.label=" ")
  return(corr)
}

corr <- corr_simple(results_corrmat)
corr_trim <-subset(corr, Freq > 0.2 | Freq <= -0.2)
corr_trim$Freq <- round(corr_trim$Freq, 3)
corr_trim <- corr_trim[corr_trim$Var1 %in% c("offset_day_mean", "offset_night_mean", "t_soil_range", "t_air_range", "cc_percent", "gps_elevation"),]
rownames(corr_trim) <- 1:nrow(corr_trim)

write.csv(corr_trim, "FILEPATH:Modeling/corrmat/corr_check.csv", row.names = F)

corr_renamed <- rename(results_corrmat, 
                       cc = cc_percent,
                       elev = gps_elevation,
                       pad_mn = pad_mean,
                       FHD_mn = FHD_mean,
                       FHD_mx = FHD_max,
                       CR_mn = CR_mean,
                       CR_mx = CR_max,
                       pai_mn = pai_mean,
                       pai_mx = pai_max,
                       RH25_mn = RH25_mean,
                       RH50_mn = RH50_mean,
                       RH95_mn = RH95_mean,
                       ENL2_mn = ENL2_mean,
                       ENL2_mx = ENL2_max
)


# Step 1: Call the pdf command to start the plot
pdf(file = "FILEPATH:/Modeling/corrmat/corrmat_spearman_offset.pdf", width = 10  ,
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
corr <- corr_simple(corr_renamed)

# Step 3: Run dev.off() to create the file!
dev.off()

corr <- corr_simple(corr_renamed)
##############################################
####BASE R PLOT TO CHECK OUT LINEAR MODELS####
##############################################
plot(corr_renamed$air ~ corr_renamed$FHD_mn, pch = 16)
# Vertical grid
axis(1, tck = 1, lty = 2, col = "gray")
# Horizontal grid  
axis(2, tck = 1, lty = 2, col = "gray")
par(new = TRUE)
plot(corr_renamed$air ~ corr_renamed$FHD_mn, pch = 16)
lm <- lm(air ~ FHD_mn, data = corr_renamed)
summary(lm)
abline(lm)

##############################################
##############################################
#############LMS OF FHD ENL AND CC############
##############################################

airpicto <- readPNG("FILEPATH:/figures/airpicto2_transparent.png")
soilpicto <- readPNG("FILEPATH:/figures/soilpicto2_transparent.png")


lm_FHD_air <- lm(air ~ FHD_mn, data = corr_renamed)
lm_ENL_air <- lm(air ~ ENL2_mn, data = corr_renamed)
lm_CC_air <- lm(air ~ cc, data = corr_renamed)
lm_FHD_soil <- lm(sor ~ FHD_mn, data = corr_renamed)
lm_ENL_soil <- lm(sor ~ ENL2_mn, data = corr_renamed)
lm_CC_soil <- lm(sor ~ cc, data = corr_renamed)

# air_FHD_agg <- aggregate(air ~ FHD_mn, data=corr_renamed, FUN=mean)
# lm_ENL2_air_agg <- lm(sor ~ cc, data = sor_CC_agg)
# summary(lm_ENL2_air_agg)

#### AGGREGATION TO FIND NUMERIC MEAN RANGE AND CORRELATE WITH THAT
air_ENL2_agg <- aggregate(air ~ ENL2_mn, data=corr_renamed, FUN=mean)
air_CC_agg <- aggregate(air ~ cc, data=corr_renamed, FUN=mean)
air_FHD_agg <- aggregate(air ~ FHD_mn, data=corr_renamed, FUN=mean)
sor_FHD_agg <- aggregate(sor ~ FHD_mn, data=corr_renamed, FUN=mean)
sor_ENL2_agg <- aggregate(sor ~ ENL2_mn, data=corr_renamed, FUN=mean)
sor_CC_agg <- aggregate(sor ~ cc, data=corr_renamed, FUN=mean)



fhd_air <- ggplot(corr_renamed, aes(x = FHD_mn, y = air)) + 
  annotation_raster(airpicto, xmin = 1, xmax = 1.6, 
                    ymin = 30, ymax = 35) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_FHD_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_FHD_air)[["FHD_mn"]], 
              intercept = coef(lm_FHD_air)[["(Intercept)"]])+
  labs(x = "Mean FHD", y = "", title = "Air Temperature")+
  annotate("text", x = 1.78, y = 30, 
           hjust="left",
           label = "R^2 == 0.32",
           parse = TRUE)+
  annotate("text", 
           x = 1.78, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.54",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
fhd_air
enl_air <- ggplot(corr_renamed, aes(x = ENL2_mn, y = air)) + 
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_ENL2_agg,color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_ENL_air)[["ENL2_mn"]], 
              intercept = coef(lm_ENL_air)[["(Intercept)"]])+
  labs(x = "Mean ENL", y = "")+
  annotate("text", x = 6, y = 30, 
           hjust="left",
           label = "R^2 == 0.29",
           parse = TRUE)+
  annotate("text", 
           x = 6, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.48",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
cc_air <- ggplot(corr_renamed, aes(x = cc, y = air)) + 
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_CC_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_CC_air)[["cc"]], 
              intercept = coef(lm_CC_air)[["(Intercept)"]])+
  labs(x = "CC (%)", y = "")+
  annotate("text", x = 73, y = 30, 
           hjust="left",
           label = "R^2 == 0.27",
           parse = TRUE)+
  annotate("text", 
           x = 73, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.50",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
fhd_soil <- ggplot(corr_renamed, aes(x = FHD_mn, y = sor)) + 
  annotation_raster(soilpicto, xmin = 1, xmax = 1.6, 
                    ymin = 30, ymax = 35) +
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_FHD_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_FHD_soil)[["FHD_mn"]], 
              intercept = coef(lm_FHD_soil)[["(Intercept)"]])+
  labs(x = "Mean FHD", title = "Soil Temperature", y = "")+
  annotate("text", x = 1.78, y = 30, 
           hjust="left",
           label = "R^2 == 0.39",
           parse = TRUE)+
  annotate("text", 
           x = 1.78, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.67",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
enl_soil <- ggplot(corr_renamed, aes(x = ENL2_mn, y = sor)) + 
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_ENL2_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_ENL_soil)[["ENL2_mn"]], 
              intercept = coef(lm_ENL_soil)[["(Intercept)"]])+
  labs(x = "Mean ENL", y = "")+
  annotate("text", x = 6, y = 30, 
           hjust="left",
           label = "R^2 == 0.33",
           parse = TRUE)+
  annotate("text", 
           x = 6, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.56",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
enl_soil
cc_soil <- ggplot(corr_renamed, aes(x = cc, y = sor)) + 
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_CC_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_CC_soil)[["cc"]], 
              intercept = coef(lm_CC_soil)[["(Intercept)"]])+
  labs(x = "CC (%)", y = "")+
  annotate("text", x = 73, y = 30, 
           hjust="left",
           label = "R^2 == 0.40",
           parse = TRUE)+
  annotate("text", 
           x = 73, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.62",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
cc_soil
######################
#legend <- cowplot::get_legend(fhd_air)
fig1 <- ggarrange(fhd_air, enl_air, cc_air, 
                  labels = c("A1", "A2", "A3"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)
fig2 <-  ggarrange(fhd_soil, enl_soil, cc_soil,
                     labels = c("S1", "S2", "S3"),
                     ncol = 1, nrow = 3,
                     align = "hv"
)
figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv"
)

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("* 13.06.2019 - 10.07.2019, study period: Iris et al. (2022)", color = "black",
                     #hjust = 1, x = 1, size = 10),
  left = text_grob("DTR - Daily temperature range (°C)",
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
                    #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH:/figures/lm_range/lm_range.pdf",
       width = 20,
       height = 23,
       units = "cm")

test<- cov(corr_renamed)
# TAKE A RANDOM SAMPLE OF SIZE N FROM THE DATA AND PLOT THAT
# testsample <- corr_renamed %>% 
#   #Grouping by the variable study
#   group_by(ENL2_mn) %>% 
#   #Sampling 3 observations for each study
#   sample_n(size = 5)
# 
# plot(testsample$air ~ testsample$FHD_mn, pch = 16)
# # Vertical grid
# axis(1, tck = 1, lty = 2, col = "gray")
# # Horizontal grid  
# axis(2, tck = 1, lty = 2, col = "gray")
# par(new = TRUE)
# plot(testsample$air ~ testsample$FHD_mn, pch = 16)
# lm <- lm(air ~ FHD_mn, data = testsample)
# summary(lm)
# abline(lm)




lm_PAI_air <- lm(air ~ pai_mn, data = corr_renamed)
lm_CR_air <- lm(air ~ CR_mn, data = corr_renamed)
lm_RH_air <- lm(air ~ RH95_mn, data = corr_renamed)
lm_PAI_soil <- lm(sor ~ pai_mn, data = corr_renamed)
lm_CR_soil <- lm(sor ~ CR_mn, data = corr_renamed)
lm_RH_soil <- lm(sor ~ RH95_mn, data = corr_renamed)

lm_elev_air <- lm(air ~ elev, data = corr_renamed)
lm_sor_air <- lm(sor ~ elev, data = corr_renamed)
summary(lm_RH_air)
#air_FHD_agg <- aggregate(air ~ FHD_mn, data=corr_renamed, FUN=mean)
test_lm <- lm(air ~ CR_mn, data = air_CR_agg)
# 
summary(lm_CR_soil)
summary(test_lm)
#### AGGREGATION TO FIND NUMERIC MEAN RANGE AND CORRELATE WITH THAT
air_PAI_agg <- aggregate(air ~ pai_mn, data=corr_renamed, FUN=mean)
air_CR_agg <- aggregate(air ~ CR_mn, data=corr_renamed, FUN=mean)
air_RH_agg <- aggregate(air ~ RH95_mn, data=corr_renamed, FUN=mean)
sor_PAI_agg <- aggregate(sor ~ pai_mn, data=corr_renamed, FUN=mean)
sor_CR_agg <- aggregate(sor ~ CR_mn, data=corr_renamed, FUN=mean)
sor_RH_agg <- aggregate(sor ~ RH95_mn, data=corr_renamed, FUN=mean)


pai_air <- ggplot(corr_renamed, aes(x = pai_mn, y = air)) + 
  annotation_raster(airpicto, xmin = 3.4, xmax = 6.6, 
                    ymin = 30, ymax = 35) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_PAI_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_PAI_air)[["pai_mn"]], 
              intercept = coef(lm_PAI_air)[["(Intercept)"]])+
  labs(x = "Mean PAI", y = "", title = "Air Temperature")+
  annotate("text", x = 7.5, y = 30, 
           hjust="left",
           label = "R^2 == 0.24",
           parse = TRUE)+
  annotate("text", 
           x = 7.5, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.39",
           color = "red",
           parse = TRUE)+
  ylim(0,35)

pai_soil <- ggplot(corr_renamed, aes(x = pai_mn, y = sor)) + 
  annotation_raster(soilpicto, xmin = 3.4, xmax = 6.6, 
                    ymin = 30, ymax = 35) +
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_PAI_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_PAI_soil)[["pai_mn"]], 
              intercept = coef(lm_PAI_soil)[["(Intercept)"]])+
  labs(x = "Mean PAI", title = "Soil Temperature", y = "")+
  annotate("text", x = 7.5, y = 30, 
           hjust="left",
           label = "R^2 == 0.31",
           parse = TRUE)+
  annotate("text", 
           x = 7.5, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.53",
           color = "red",
           parse = TRUE)+
  ylim(0,35)


figure <- ggarrange(pai_air, pai_soil,
                    ncol = 2, nrow = 1,
                    labels = c("A1", "S1"),
                    align = "hv"
)

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("Mean PAI", color = "black"),
  left = text_grob("DTR - Daily temperature range (°C)",
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH/figures/lm_range/lm_range_pai.pdf",
       width = 23,
       height = 10,
       units = "cm")


cr_air <- ggplot(corr_renamed, aes(x = CR_mn, y = air)) + 
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_CR_agg,color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_CR_air)[["CR_mn"]], 
              intercept = coef(lm_CR_air)[["(Intercept)"]])+
  labs(x = "Mean CR", y = "")+
  annotate("text", x = 0.68, y = 30, 
           hjust="left",
           label = "R^2 == 0.04",
           parse = TRUE)+
  annotate("text", 
           x = 0.68, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.02 (i)",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
rh_air <- ggplot(corr_renamed, aes(x = RH95_mn, y = air)) + 
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_RH_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_RH_air)[["RH95_mn"]], 
              intercept = coef(lm_RH_air)[["(Intercept)"]])+
  labs(x = "Mean RH95", y = "")+
  annotate("text", x = 16, y = 30, 
           hjust="left",
           label = "R^2 == 0.23",
           parse = TRUE)+
  annotate("text", 
           x = 16, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.38",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
pai_soil <- ggplot(corr_renamed, aes(x = pai_mn, y = sor)) + 
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_PAI_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_PAI_soil)[["pai_mn"]], 
              intercept = coef(lm_PAI_soil)[["(Intercept)"]])+
  labs(x = "Mean PAI", title = "Soil Temperature", y = "")+
  annotate("text", x = 7.5, y = 30, 
           hjust="left",
           label = "R^2 == 0.31",
           parse = TRUE)+
  annotate("text", 
           x = 7.5, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.53",
           color = "red",
           parse = TRUE)+
  ylim(0,35)
cr_soil <- ggplot(corr_renamed, aes(x = CR_mn, y = sor)) + 
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_CR_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_CR_soil)[["CR_mn"]], 
              intercept = coef(lm_CR_soil)[["(Intercept)"]])+
  labs(x = "Mean CR", y = "")+
  annotate("text", x = 0.68, y = 30, 
           hjust="left",
           label = "R^2 == 0.11",
           parse = TRUE)+
  annotate("text", 
           x = 0.68, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.15 (i)",
           color = "red",
           parse = TRUE)+
  ylim(0,35)

rh_soil <- ggplot(corr_renamed, aes(x = RH95_mn, y = sor)) + 
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_RH_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_RH_soil)[["RH95_mn"]], 
              intercept = coef(lm_RH_soil)[["(Intercept)"]])+
  labs(x = "Mean RH95", y = "")+
  annotate("text", x = 16, y = 30, 
           hjust="left",
           label = "R^2 == 0.28",
           parse = TRUE)+
  annotate("text", 
           x = 16, y = 25, 
           hjust="left",
           label = "{R^2}[mn] == 0.46",
           color = "red",
           parse = TRUE)+
  ylim(0,35)

fig1 <- ggarrange(cr_air, rh_air, 
                  labels = c("A1", "A2"),
                  ncol = 1, nrow = 2,
                  align = "hv"
)
fig2 <-  ggarrange(cr_soil, rh_soil,
                   labels = c("S1", "S2"),
                   ncol = 1, nrow = 2,
                   align = "hv"
)
figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv"
)

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("* 13.06.2019 - 10.07.2019, study period: Iris et al. (2022)", color = "black",
  #hjust = 1, x = 1, size = 10),
  left = text_grob("DTR - Daily temperature range (°C)",
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH/figures/lm_range/lm_range_crrh.pdf",
       width = 20,
       height = 15,
       units = "cm")

##########################################
#######FIGURE ELEV, OFFSET DAY NIGHT
##########################################
lm_elev_air <- lm(air ~ elev, data = corr_renamed)
lm_elev_soil <- lm(sor ~ elev, data = corr_renamed)
air_elev_agg <- aggregate(air ~ elev, data=corr_renamed, FUN=mean)
sor_elev_agg <- aggregate(sor ~ elev, data=corr_renamed, FUN=mean)

elev_air <- ggplot(corr_renamed, aes(x = elev, y = air)) + 
  annotation_raster(airpicto, xmin = 1100, xmax = 1400, 
                    ymin = 30, ymax = 35) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = air_elev_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_elev_air)[["elev"]], 
              intercept = coef(lm_elev_air)[["(Intercept)"]])+
  labs(x = "", y = "", title = "Air Temperature")+
  annotate("text", x = 1500, y = 32, 
           hjust="left",
           label = "R^2 == 0.21",
           parse = TRUE)+
  annotate("text", 
           x = 1500, y = 27, 
           hjust="left",
           label = "{R^2}[mn] == 0.34",
           color = "red",
           parse = TRUE)+
  ylim(0,35)

elev_soil <- ggplot(corr_renamed, aes(x = elev, y = sor)) + 
  annotation_raster(soilpicto, xmin = 1100, xmax = 1400, 
                    ymin = 30, ymax = 35) +
  geom_point(color ="#0C7BDC", alpha = 0.2) + 
  geom_point(data = sor_elev_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_elev_soil)[["elev"]], 
              intercept = coef(lm_elev_soil)[["(Intercept)"]])+
  labs(x = "", title = "Soil Temperature", y = "")+
  annotate("text", x = 1500, y = 32, 
           hjust="left",
           label = "R^2 == 0.07",
           parse = TRUE)+
  annotate("text", 
           x = 1500, y = 27, 
           hjust="left",
           label = "{R^2}[mn] == 0.06(i)",
           color = "red",
           parse = TRUE)+
  ylim(0,35)

figure <- ggarrange(elev_air, elev_soil,
                    labels = c("A1", "S1"),
                    ncol = 2, nrow = 1,
                    align = "hv"
)

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  bottom = text_grob("Elevation (m a.s.l.)", color = "black",),
  left = text_grob("DTR - Daily temperature range (°C)",
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  #fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATHfigures/lm_range/lm_elev_new.pdf",
       width = 23,
       height = 10,
       units = "cm")
####################################################
#######OFFSET DAY NIGHT, MIN, MAX, MED FOR AIR TEMPS
####################################################
daypicto <- readPNG("FILEPATH/figures/daypicto_transparent.png")
nightpicto <- readPNG("FILEPATH/figures/nightpicto_transparent.png")

lm_offd_max <- lm(t_air_range ~ offd_max, data = results_corrmat)
lm_offd_med <- lm(t_air_range ~ offd_med, data = results_corrmat)
lm_offd_min <- lm(t_air_range ~ offd_min, data = results_corrmat)

lm_offn_max <- lm(t_air_range ~ offn_max, data = results_corrmat)
lm_offn_med <- lm(t_air_range ~ offn_med, data = results_corrmat)
lm_offn_min <- lm(t_air_range ~ offn_min, data = results_corrmat)

summary(lm_offd_med)


offd_max <- ggplot(results_corrmat, aes(x = offd_max, y = t_air_range)) +
  annotation_raster(daypicto, xmin = -21.5, xmax = -9, 
                    ymin = 0, ymax = 7) +
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                       high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="bottom")+
  geom_abline(slope = coef(lm_offd_max)[["offd_max"]], 
              intercept = coef(lm_offd_max)[["(Intercept)"]])+
  labs(x = "...Maximum temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.69",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)
offd_max
legend <- get_legend(offd_max)
offd_max <- ggplot(results_corrmat, aes(x = offd_max, y = t_air_range)) +
  annotation_raster(daypicto, xmin = -21.5, xmax = -9, 
                    ymin = 0, ymax = 7) +
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_offd_max)[["offd_max"]], 
              intercept = coef(lm_offd_max)[["(Intercept)"]])+
  labs(x = "...maximum air temperatures to ERA5 (°C)", title  = "Offset of daytime (06:00 - 18:00)...", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.69",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)
#done
offd_med <- ggplot(results_corrmat, aes(x = offd_med, y = t_air_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_offd_med)[["offd_med"]], 
              intercept = coef(lm_offd_med)[["(Intercept)"]])+
  labs(x = "...median air temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.46",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offd_min <- ggplot(results_corrmat, aes(x = offd_min, y = t_air_range)) + 
   geom_bin2d(bins = 70) +
   scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_offd_min)[["offd_min"]], 
              intercept = coef(lm_offd_min)[["(Intercept)"]])+
  labs(x = "...minimum air temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.06",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offn_max <- ggplot(results_corrmat, aes(x = offn_max, y = t_air_range)) + 
  annotation_raster(nightpicto, xmin = -21.5, xmax = -8, 
                    ymin = 0, ymax = 7) +
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  geom_abline(slope = coef(lm_offn_max)[["offn_max"]], 
              intercept = coef(lm_offn_max)[["(Intercept)"]])+
  labs(x = "...maximum air temperatures to ERA5 (°C)", title = "Offset of nightly (19:00 - 05:00)...", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.23",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offn_med <- ggplot(results_corrmat, aes(x = offn_med, y = t_air_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  geom_abline(slope = coef(lm_offn_med)[["offn_med"]], 
              intercept = coef(lm_offn_med)[["(Intercept)"]])+
  labs(x = "...median air temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.04",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offn_min <- ggplot(results_corrmat, aes(x = offn_min, y = t_air_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  geom_abline(slope = coef(lm_offn_min)[["offn_min"]], 
              intercept = coef(lm_offn_min)[["(Intercept)"]])+
  labs(x = "...minimum air temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0 (i)",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

fig1 <- ggarrange(offd_max, offd_med, offd_min, 
                  labels = c("D1", "D2", "D3"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)
fig2 <-  ggarrange(offn_max, offn_med, offn_min,
                   labels = c("N1", "N2", "N3"),
                   ncol = 1, nrow = 3,
                   align = "hv"
)
figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv",
                    common.legend = T,
                    legend.grob = legend,
                    legend = "bottom"
)

## ARRANGE INDIVIDUAL PLOTS WITH PARSER

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("* 13.06.2019 - 10.07.2019, study period: Iris et al. (2022)", color = "black",
  #hjust = 1, x = 1, size = 10),
  left = text_grob(bquote(""~DTR[a]~"- Daily air temperature range (°C)"),
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH/figures/lm_range/lm_offs_air.pdf",
       width = 20,
       height = 23,
       units = "cm")
# 

####################################################
#######OFFSET DAY NIGHT, MIN, MAX, MED FOR SOIL TEMPS
####################################################
soilpicto <- readPNG("FILEPATH/figures/soilpicto_transparent.png")

lm_offd_max <- lm(t_soil_range ~ offd_max, data = results_corrmat)
lm_offd_med <- lm(t_soil_range ~ offd_med, data = results_corrmat)
lm_offd_min <- lm(t_soil_range ~ offd_min, data = results_corrmat)

lm_offn_max <- lm(t_soil_range ~ offn_max, data = results_corrmat)
lm_offn_med <- lm(t_soil_range ~ offn_med, data = results_corrmat)
lm_offn_min <- lm(t_soil_range ~ offn_min, data = results_corrmat)

summary(lm_offn_min)


offd_max <- ggplot(results_corrmat, aes(x = offd_max, y = t_soil_range)) +
  annotation_raster(soilpicto, xmin = 9, xmax = 16, 
                    ymin = 17, ymax = 28) +
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="bottom")+
  geom_abline(slope = coef(lm_offd_max)[["offd_max"]], 
              intercept = coef(lm_offd_max)[["(Intercept)"]])+
  labs(x = "...Maximum Temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.32",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)
offd_max
legend <- get_legend(offd_max)
offd_max <- ggplot(results_corrmat, aes(x = offd_max, y = t_soil_range)) +
  annotation_raster(soilpicto, xmin = 9, xmax = 16, 
                    ymin = 17, ymax = 28) +
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_offd_max)[["offd_max"]], 
              intercept = coef(lm_offd_max)[["(Intercept)"]])+
  labs(x = "...Maximum Soil Temperatures to ERA5 (°C)", title  = "Offset of Diurnal...", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.32",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)
#done
offd_med <- ggplot(results_corrmat, aes(x = offd_med, y = t_soil_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_offd_med)[["offd_med"]], 
              intercept = coef(lm_offd_med)[["(Intercept)"]])+
  labs(x = "...Median Soil Temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.24",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offd_min <- ggplot(results_corrmat, aes(x = offd_min, y = t_soil_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_offd_min)[["offd_min"]], 
              intercept = coef(lm_offd_min)[["(Intercept)"]])+
  labs(x = "...Minimum Soil Temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.02",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offn_max <- ggplot(results_corrmat, aes(x = offn_max, y = t_soil_range)) + 
  annotation_raster(soilpicto, xmin = 9, xmax = 16, 
                    ymin = 17, ymax = 28) +
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  geom_abline(slope = coef(lm_offn_max)[["offn_max"]], 
              intercept = coef(lm_offn_max)[["(Intercept)"]])+
  labs(x = "...Maximum Soil Temperatures to ERA5 (°C)", title = "Offset of Nightly...", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.16",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offn_med <- ggplot(results_corrmat, aes(x = offn_med, y = t_soil_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  geom_abline(slope = coef(lm_offn_med)[["offn_med"]], 
              intercept = coef(lm_offn_med)[["(Intercept)"]])+
  labs(x = "...Median Soil Temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0.01",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

offn_min <- ggplot(results_corrmat, aes(x = offn_min, y = t_soil_range)) + 
  geom_bin2d(bins = 70) +
  scale_fill_gradient(low = "purple",
                      high = "yellow")+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  geom_abline(slope = coef(lm_offn_min)[["offn_min"]], 
              intercept = coef(lm_offn_min)[["(Intercept)"]])+
  labs(x = "...Minimum Soil temperatures to ERA5 (°C)", y = "")+
  annotate("text", x = 6, y = 32, 
           hjust="left",
           label = "R^2 == 0 (i)",
           parse = TRUE)+
  ylim(0,35)+
  xlim(-20,15)

fig1 <- ggarrange(offd_max, offd_med, offd_min, 
                  labels = c("D1", "D2", "D3"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)
fig2 <-  ggarrange(offn_max, offn_med, offn_min,
                   labels = c("N1", "N2", "N3"),
                   ncol = 1, nrow = 3,
                   align = "hv"
)
figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv",
                    common.legend = T,
                    legend.grob = legend,
                    legend = "bottom"
)

## ARRANGE INDIVIDUAL PLOTS WITH PARSER

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("* 13.06.2019 - 10.07.2019, study period: Iris et al. (2022)", color = "black",
  #hjust = 1, x = 1, size = 10),
  left = text_grob("Daily temperature range (°C)",
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH/figures/lm_range/lm_offs_soil.pdf",
       width = 20,
       height = 23,
       units = "cm")
# 


######MODELING
# remove offn, cc,
all_var_lm <- lm(formula = air ~ 
       offd +
       elev +
       # FHD_mn+
       # FHD_mx +
       # cc +
       pad_mn + 
       # pad_mx + 
       CR_mn +
       CR_mx +
       # pai_mn +
       # pai_mx + 
       # RH95_mn +
       RH95_mx +
       RH25_mx 
       # RH25_mn +
       # RH50_mn 
       # RH50_mx 
       # ENL2_mn 
       # ENL2_mx
       , 
     data = corr_renamed)
summary(all_var_lm)
vif(all_var_lm)

cc_lm <- lm(formula = air ~ pai_mn, data = corr_renamed)
summary(cc_lm)

some_var_lm <- lm(formula = air ~ FHD_mn , data = corr_unique)
summary(some_var_lm)
vif(some_var_lm)

air_agg <- aggregate(air ~ elev, data=corr_renamed, FUN=mean)
corr_unique <- unique(corr_renamed[,-c(1:4)])
corr_unique <- left_join(corr_unique, air_agg, by = "elev")



##########################################
#######OFFSET DAY NIGHT FHD ELEV ENL2#####
##########################################
crescent_img <- readPNG("FILEPATH/figures/crescent.png")
sun_img <- readPNG("FILEPATH/figures/sun.png")


lm_elev_offd <- lm(offd_med ~ elev, data = corr_renamed)
lm_elev_offn <- lm(offn_med ~ elev, data = corr_renamed)
lm_FHD_offd <- lm(offd_med ~ FHD_mn, data = corr_renamed)
lm_FHD_offn <- lm(offn_med ~ FHD_mn, data = corr_renamed)
lm_ENL_offd <- lm(offd_med ~ ENL2_mn, data = corr_renamed)
lm_ENL_offn <- lm(offn_med ~ ENL2_mn, data = corr_renamed)
#air_FHD_agg <- aggregate(air ~ FHD_mn, data=corr_renamed, FUN=mean)
test_lm <- lm(offn_med~ ENL2_mn, data = offn_ENL_agg)
summary(lm_ENL_offn)
summary(test_lm)


summary(test_lm)

#### AGGREGATION TO FIND NUMERIC MEAN RANGE AND CORRELATE WITH THAT
offd_elev_agg <- aggregate(offd_med ~ elev, data=corr_renamed, FUN=mean)
offn_elev_agg <- aggregate(offn_med ~ elev, data=corr_renamed, FUN=mean)
offd_FHD_agg <- aggregate(offd_med ~ FHD_mn, data=corr_renamed, FUN=mean)
offn_FHD_agg <- aggregate(offn_med ~ FHD_mn, data=corr_renamed, FUN=mean)
offd_ENL_agg <- aggregate(offd_med ~ ENL2_mn, data=corr_renamed, FUN=mean)
offn_ENL_agg <- aggregate(offn_med ~ ENL2_mn, data=corr_renamed, FUN=mean)


elev_offd <- ggplot(corr_renamed, aes(x = elev, y = offd_med)) + 
  annotation_raster(daypicto, xmin = 1550, xmax = 1800, 
                    ymin = -15, ymax = -10) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = offd_elev_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_elev_offd)[["elev"]], 
              intercept = coef(lm_elev_offd)[["(Intercept)"]])+
  labs(x = "Elevation (m a.s.l.)", y = "", title = "Daytime Temperature (06:00 - 18:00)")+
  annotate("text", x = 1000, y = 17, 
           hjust="left",
           label = "R^2 == 0.7",
           parse = TRUE)+
  annotate("text", 
           x = 1000, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.79",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)
elev_offd

FHD_offd <- ggplot(corr_renamed, aes(x = FHD_mn, y = offd_med)) + 
  # annotation_raster(sun_img, xmin = 1.85, xmax = 2.2, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = offd_FHD_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_FHD_offd)[["FHD_mn"]], 
              intercept = coef(lm_FHD_offd)[["(Intercept)"]])+
  labs(x = "Mean FHD", y = "")+
  annotate("text", x = 0.6, y = 17, 
           hjust="left",
           label = "R^2 == 0.19",
           parse = TRUE)+
  annotate("text", x = 0.6, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.17 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)


ENL_offd <- ggplot(corr_renamed, aes(x = ENL2_mn, y = offd_med)) + 
  # annotation_raster(sun_img, xmin = 6.25, xmax = 7.4, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = offd_ENL_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_ENL_offd)[["ENL2_mn"]], 
              intercept = coef(lm_ENL_offd)[["(Intercept)"]])+
  labs(x = "Mean ENL", y = "")+
  annotate("text", x = 2.2, y = 17, 
           hjust="left",
           label = "R^2 == 0.18",
           parse = TRUE)+
  annotate("text", 
           x = 2.2, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.14 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

elev_offn <- ggplot(corr_renamed, aes(x = elev, y = offn_med)) + 
  annotation_raster(nightpicto, xmin = 1500, xmax = 1800, 
                    ymin = -15, ymax = -10) +
  geom_point(color = "#0C7BDC", alpha = 0.2) + 
  geom_point(data = offn_elev_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold")
        # ,plot.background = element_rect(fill = "#0C7BDC", alpha = 0.5)
        )+
  geom_abline(slope = coef(lm_elev_offn)[["elev"]], 
              intercept = coef(lm_elev_offn)[["(Intercept)"]])+
  labs(x = "Elevation (m a.s.l.)", y = "", title = "Nightly Temperature (19:00 - 5:00)")+
  annotate("text", x = 1000, y = 17, 
           hjust="left",
           label = "R^2 == 0.72",
           parse = TRUE)+
  annotate("text", 
           x = 1000, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.82",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

FHD_offn <- ggplot(corr_renamed, aes(x = FHD_mn, y = offn_med)) + 
  # annotation_raster(crescent_img, xmin = 1.85, xmax = 2.2, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#0C7BDC", alpha = 0.2) + 
  geom_point(data = offn_FHD_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold")
        # ,plot.background = element_rect(fill = "#0C7BDC", alpha = 0.5)
        )+
  geom_abline(slope = coef(lm_FHD_offn)[["FHD_mn"]], 
              intercept = coef(lm_FHD_offn)[["(Intercept)"]])+
  labs(x = "Mean FHD", y = "")+
  annotate("text", x = 0.6, y = 17, 
           hjust="left",
           label = "R^2 == 0.01",
           parse = TRUE)+
  annotate("text", x = 0.6, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.01 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)


ENL_offn <- ggplot(corr_renamed, aes(x = ENL2_mn, y = offn_med)) + 
  # annotation_raster(crescent_img, xmin = 6.3, xmax = 7.4, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#0C7BDC", alpha = 0.2) + 
  geom_point(data = offn_ENL_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold")
        # ,plot.background = element_rect(fill = "#0C7BDC", alpha = 0.5)
        )+
  geom_abline(slope = coef(lm_ENL_offn)[["ENL2_mn"]], 
              intercept = coef(lm_ENL_offn)[["(Intercept)"]])+
  labs(x = "Mean ENL", y = "")+
  annotate("text", x = 2.2, y = 17, 
           hjust="left",
           label = "R^2 == 0.02",
           parse = TRUE)+
  annotate("text", 
           x = 2.2, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.01 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

fig1 <- ggarrange(elev_offd, FHD_offd, ENL_offd, 
                  labels = c("D1", "D2", "D3"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)

fig2 <- ggarrange(elev_offn, FHD_offn, ENL_offn, 
                  labels = c("N1", "N2", "N3"),
                  ncol = 1, nrow = 3,
                  align = "hv"
)


figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv"
)

## ARRANGE INDIVIDUAL PLOTS WITH PARSER

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("* 13.06.2019 - 10.07.2019, study period: Iris et al. (2022)", color = "black",
  #hjust = 1, x = 1, size = 10),
  left = text_grob(bquote(""~Ot[med]~"- Offset of median temperatures to ERA5 (°C)"),
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH/figures/lm_range/lm_offs_fhd.pdf",
       width = 20,
       height = 23,
       units = "cm")
# 
# legend <- get_legend(offn_soil)


##########################################
#######OFFSET DAY NIGHT CC PAI MN PAI MX##
##########################################
crescent_img <- readPNG("FILEPATH/figures/crescent.png")
sun_img <- readPNG("FILEPATH/figures/sun.png")

lm_cc_offd <- lm(offd_med ~ cc, data = corr_renamed)
lm_cc_offn <- lm(offn_med ~ cc, data = corr_renamed)
lm_paimn_offd <- lm(offd_med ~ pai_mn, data = corr_renamed)
lm_paimn_offn <- lm(offn_med ~ pai_mn, data = corr_renamed)
lm_paimx_offd <- lm(offd_med ~ pai_mx, data = corr_renamed)
lm_paimx_offn <- lm(offn_med ~ pai_mx, data = corr_renamed)
#air_FHD_agg <- aggregate(air ~ FHD_mn, data=corr_renamed, FUN=mean)
test_lm <- lm(offd_med~ pai_mn, data = offd_paimn_agg)
summary(lm_paimn_offd)
summary(test_lm)

#### AGGREGATION TO FIND NUMERIC MEAN RANGE AND CORRELATE WITH THAT
offd_cc_agg <- aggregate(offd_med ~ cc, data=corr_renamed, FUN=mean)
offn_cc_agg <- aggregate(offn_med ~ cc, data=corr_renamed, FUN=mean)
offd_paimn_agg <- aggregate(offd_med ~ pai_mn, data=corr_renamed, FUN=mean)
offn_paimn_agg <- aggregate(offn_med ~ pai_mn, data=corr_renamed, FUN=mean)
offd_paimx_agg <- aggregate(offd_med ~ pai_mx, data=corr_renamed, FUN=mean)
offn_paimx_agg <- aggregate(offn_med ~ pai_mx, data=corr_renamed, FUN=mean)


cc_offd <- ggplot(corr_renamed, aes(x = cc, y = offd_med)) + 
  annotation_raster(daypicto, xmin = 70, xmax = 96, 
                    ymin = -15, ymax = -10) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = offd_cc_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  geom_abline(slope = coef(lm_cc_offd)[["cc"]], 
              intercept = coef(lm_cc_offd)[["(Intercept)"]])+
  labs(x = "CC (%)", y = "", title = "Daytime Temperature (06:00 - 18:00)")+
  annotate("text", x = 12.5, y = 17, 
           hjust="left",
           label = "R^2 == 0.12",
           parse = TRUE)+
  annotate("text", 
           x = 12.5, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.02(i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

paimn_offd <- ggplot(corr_renamed, aes(x = pai_mn, y = offd_med)) + 
  # annotation_raster(sun_img, xmin = 1.85, xmax = 2.2, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = offd_paimn_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_paimn_offd)[["pai_mn"]], 
              intercept = coef(lm_paimn_offd)[["(Intercept)"]])+
  labs(x = "Mean PAI", y = "")+
  annotate("text", x = 1.26, y = 17, 
           hjust="left",
           label = "R^2 == 0.15",
           parse = TRUE)+
  annotate("text", x = 1.26, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.12 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)


paimx_offd <- ggplot(corr_renamed, aes(x = pai_mx, y = offd_med)) + 
  # annotation_raster(sun_img, xmin = 6.25, xmax = 7.4, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#E5AA09", alpha = 0.2) + 
  geom_point(data = offd_paimx_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position="none")+
  geom_abline(slope = coef(lm_paimx_offd)[["pai_mx"]], 
              intercept = coef(lm_paimx_offd)[["(Intercept)"]])+
  labs(x = "Maximum PAI", y = "")+
  annotate("text", x = 6, y = 17, 
           hjust="left",
           label = "R^2 == 0.24",
           parse = TRUE)+
  annotate("text", 
           x = 6, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.04",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

cc_offn <- ggplot(corr_renamed, aes(x = cc, y = offn_med)) + 
  annotation_raster(nightpicto, xmin = 68, xmax = 96, 
                    ymin = -15, ymax = -10) +
  geom_point(color = "#0C7BDC", alpha = 0.2) + 
  geom_point(data = offn_cc_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold")
        # ,plot.background = element_rect(fill = "#0C7BDC", alpha = 0.5)
  )+
  geom_abline(slope = coef(lm_cc_offn)[["cc"]], 
              intercept = coef(lm_cc_offn)[["(Intercept)"]])+
  labs(x = "CC (%)", y = "", title = "Nightly Temperature (19:00 - 05:00)")+
  annotate("text", x = 12.5, y = 17, 
           hjust="left",
           label = "R^2 == 0.01",
           parse = TRUE)+
  annotate("text", 
           x = 12.5, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == -0.05 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

paimn_offn <- ggplot(corr_renamed, aes(x = pai_mn, y = offn_med)) + 
  # annotation_raster(crescent_img, xmin = 1.85, xmax = 2.2, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#0C7BDC", alpha = 0.2) + 
  geom_point(data = offn_paimn_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold")
        # ,plot.background = element_rect(fill = "#0C7BDC", alpha = 0.5)
  )+
  geom_abline(slope = coef(lm_paimn_offn)[["pai_mn"]], 
              intercept = coef(lm_paimn_offn)[["(Intercept)"]])+
  labs(x = "Mean PAI", y = "")+
  annotate("text", x = 1.26, y = 17,
           hjust="left",
           label = "R^2 == 0.02",
           parse = TRUE)+
  annotate("text", x = 1.26, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == -0.07 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)


paimx_offn <- ggplot(corr_renamed, aes(x = pai_mx, y = offn_med)) + 
  # annotation_raster(crescent_img, xmin = 6.3, xmax = 7.4, 
  #                   ymin = -15, ymax = -5) +
  geom_point(color = "#0C7BDC", alpha = 0.2) + 
  geom_point(data = offn_paimx_agg, color = "black", fill ="red",  size = 2, shape = 21) +
  theme_bw()+
  theme(axis.text = element_text(face="bold")
        # ,plot.background = element_rect(fill = "#0C7BDC", alpha = 0.5)
  )+
  geom_abline(slope = coef(lm_paimx_offn)[["pai_mx"]], 
              intercept = coef(lm_paimx_offn)[["(Intercept)"]])+
  labs(x = "Maximum PAI", y = "")+
  annotate("text", x = 6, y = 17, 
           hjust="left",
           label = "R^2 == 0.07",
           parse = TRUE)+
  annotate("text", 
           x = 6, y = 13, 
           hjust="left",
           label = "{R^2}[mn] == 0.02 (i)",
           color = "red",
           parse = TRUE)+
  ylim(-15,20)

fig1 <- ggarrange(cc_offd, paimn_offd, #paimx_offd, 
                  labels = c("D1", "D2", "D3"),
                  ncol = 1, nrow = 2,
                  align = "hv"
)

fig2 <- ggarrange(cc_offn, paimn_offn, #paimx_offn, 
                  labels = c("N1", "N2", "N3"),
                  ncol = 1, nrow = 2,
                  align = "hv"
)


figure <- ggarrange(fig1, fig2,
                    ncol = 2, nrow = 1,
                    align = "hv"
                    
)

## ARRANGE INDIVIDUAL PLOTS WITH PARSER

annotate_figure(
  figure,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("* 13.06.2019 - 10.07.2019, study period: Iris et al. (2022)", color = "black",
  #hjust = 1, x = 1, size = 10),
  left = text_grob(bquote(""~Ot[med]~"- Offset of median temperatures to ERA5 (°C)"),
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave("FILEPATH/figures/lm_range/lm_offs_ccpai.pdf",
       width = 20,
       height = 15,
       units = "cm")
# 
# legend <- get_legend(offn_soil)
######MODELING
#####SORRY LINK COME BACK WHEN YOURE A LITTLEMMMMSMARTER
# remove offn, cc,
all_var_lm <- lm(formula = air ~ 
                   offd +
                   elev +
                   FHD_mn+
                 FHD_mx +
                 cc +
                 CR_mn +
                 CR_mx +
                 pai_mn +
                 pai_mx + 
                 RH95_mn +
                 RH95_mx +
                 RH25_mx +
                 RH25_mn +
                 RH50_mn +
                 RH50_mx
                 , 
                 data = corr_renamed)
summary(all_var_lm)
vif(all_var_lm)

cc_lm <- lm(formula = air ~ pai_mn, data = corr_renamed)
summary(cc_lm)

some_var_lm <- lm(formula = air ~ FHD_mn , data = corr_unique)
summary(some_var_lm)
vif(some_var_lm)

air_agg <- aggregate(air ~ elev, data=corr_renamed, FUN=mean)
corr_unique <- unique(corr_renamed[,-c(1:4)])
corr_unique <- left_join(corr_unique, air_agg, by = "elev")


