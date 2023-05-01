# install.packages("XYZ")
require(tidyverse)
require(ggplot2)
require(ggpubr)
require(dplyr)
require(tidyverse)
require(caret)
require(leaps)
require(MASS)
require(glmnet)
require(effects)
require(viridis)
require(car)
require(stargazer)
require(effectsize)
library(effsize)
###############################
####PREPARATION: COMMENTED OUT#
###############################

## LOAD AND MERGE MICROCLIMATE 
# microclimate_hourly <- read.csv("FILEPATH/Microclimate/hourly_summary_all_temps/all_plots_hourly_summary_all_temps.csv")
# microclimate_hourly <- rename(microclimate_hourly, las_name = plot)

# just in case these are needed
# ecology_metrics <- read.csv("FILEPATH/figures/temp_vs_metric/other_metrics_sorted.csv")
# pad_metrics <- read.csv("FILEPATH/figures/temp_vs_metric/total.pad.stat.summarized.csv")
# enl_metrics <- read.csv("FILEPATH/ENL/summary_metrics/summary_ENL.csv")


# LOAD OLD SUMMARY DF
# stats_plus_micro <- read.csv("FILEPATH/figures/temp_vs_metric/stats_plus_micro_v2.csv")
# # DELETE TEMP ROWS (IRRELEVANT BECAUSE WE DONT GO OFF SUMMARY METRICS NOW (?)
# stats_plus_micro <- stats_plus_micro[,-c(1,3)]
# stats_plus_micro <- stats_plus_micro[,-c(2:15)]
# stats_plus_micro_unique <- unique(stats_plus_micro)
# # FOR SOME REASON LAS 17 IS TWO TIMES WITHIN THIS DF! INVESTIGATE PS:
# # METRICS LOOK FINE, ROW 17 IS TO BE REMOVED, POSSIBLE REASON: ROUNDING ERROR?
# stats_plus_micro_unique <- stats_plus_micro_unique[-17,]
# model_df <- merge(microclimate_hourly, stats_plus_micro_unique, by='las_name', all.x=TRUE)
# write.csv(model_df, "FILEPATH/modeling/model_df.csv",row.names = F)

# ADAPT ERA5 DATA TO NEW FORMAT AND THEN ROWBIND THEM INTO COMMON DATAFRAME
# las_name = "las_17"
# era5 <- read.csv(paste("FILEPATH/Macroclimate/ERA5_corrected_temperature/", las_name, "_AT2M_corrected.csv", sep = ""))
# era5 <- era5[,c(1,2)]
# era5$las_name <- las_name
# era5 <- rename(era5,
#                t_air_ERA5 = AT2m_corrected,
#                hourofday = TIMESTAMP)
# write.csv(era5, paste("FILEPATH/modeling/era5/", las_name, "_era5_for_model_df.csv", sep = ""), row.names = F)
# # APPLY THEM INTO A LIST
# my_files <- list.files("FILEPATH/modeling/era5/", pattern = "\\.csv$")
# # CHANGE WD FOR LAPPLY
# setwd("FILEPATH/modeling/era5/")
# # READ ALL FILES
# sum_metric_list <- lapply(my_files, read.csv)
# # COMBINE FILES
# all_sum_metrics <- bind_rows(sum_metric_list)
# #all_sum_metrics <- all_sum_metrics[,-1]
# write.csv(all_sum_metrics, "FILEPATH/modeling/era5/all_era5_for_model.csv", row.names = F)

# CONVERT DATETIME TO POSIXCT JUST IN CASE IT MATTERS FOR DPLYR FILTER
# all_sum_metrics$hourofday <- as.POSIXct(all_sum_metrics$hourofday)
# model_df$hourofday <- as.POSIXct(model_df$hourofday)
# EVERYTHING BEFORE 13.06 IS NONSENSE DATA
# model_df_filtered <- model_df %>% filter(hourofday >= "2019-06-13" & hourofday < "2020-02-05")
# era5_filtered <- all_sum_metrics %>% filter(hourofday >= "2019-06-13" & hourofday < "2020-02-05")
# WRITE 
# write.csv(model_df_filtered, "FILEPATH/modeling/filtered/model_df_filtered.csv", row.names = F)
# write.csv(era5_filtered, "FILEPATH/modeling/filtered/era5_filtered.csv", row.names = F)

# READ MICROCLIMATE AND ERA5 DATA
model_df <- read.csv("FILEPATH/modeling/filtered/model_df_filtered.csv")
era5_df <- read.csv("FILEPATH/modeling/filtered/era5_filtered.csv")

data_merge1 <- merge(model_df, era5_df, by = c("las_name", "hourofday")) 

# TEST IF COLUMN-BIND LEADS TO CORRECT VALUE TO VALUE PLACEMENT (IT DOES)
# test <- cbind(era5_df, model_df)
# tester<- ifelse(test[,1]==test[,5],"Yes","No")
# any(tester== "No")
# tester<- ifelse(test[,3]==test[,4],"Yes","No")
# any(tester== "No")

model_df <- cbind(era5_df, model_df)
model_df <- model_df[,-c(1,3)]
model_df <- model_df %>% relocate(t_air_ERA5, .after = t_air)
model_df$t_air_offset <- model_df$t_air_ERA5 - model_df$t_air
model_df <- model_df %>% relocate(t_air_offset, .after = t_air_ERA5)

write.csv(model_df, "FILEPATH/Modeling/model_df_2_1.csv", row.names = F)
plot(model_df$t_air_offset ~ model_df$ENL1_mean)
abline(model)
####################
#####MODELING#######
###################

########
###what about data normalization?
###what about independence of observations, most of these 
###values are pad/CR derived iirc: check for correlation
###normality?
model <- lm(t_air_offset ~ ENL1_mean, data = model_df)
summary(model)
plot(model_df$t_air_offset ~ model_df$ENL1_mean)
abline(model)

plot(model)

# ggplot(data=model_df, aes(x=RH95_mean, y=t_air_offset)) +
#   geom_smooth(method="lm") +
#   geom_point() +
#   stat_regline_equation(label.x=30, label.y=310)


###YE OLDE DAYNIGHT EXTRACTION###
# THIS TIME WITHOUT A FUNCTION
# extract daytime
# model_df[,2] <- as.POSIXct(model_df[,2], tz = "UTC")
# model_df$hms <- hms::as_hms(model_df[,2])
# model_df_day <- model_df %>% dplyr::select(everything()) %>% 
#    dplyr::filter(
#      hms >= hms::as_hms('05:59:00'),
#      hms <= hms::as_hms('18:01:00'))
# # extract nighttime
# model_df_nighttime1 <- model_df %>% dplyr::select(everything()) %>% 
#   dplyr::filter(hms >= hms::as_hms('18:01:00'))
# model_df_nighttime2 <- model_df %>% dplyr::select(everything()) %>% 
#    dplyr::filter(hms <= hms::as_hms('05:59:00'))
# model_df_nighttime_bind <- rbind(model_df_nighttime1, model_df_nighttime2)
# model_df_night <- model_df_nighttime_bind %>% dplyr::select(everything()) %>% 
#    arrange(model_df_nighttime_bind$hourofday)
# 
# write.csv(model_df_night, "FILEPATH/modeling/model_df_night.csv", row.names = F)
# write.csv(model_df_day, "FILEPATH/modeling/model_df_day.csv", row.names = F)
model_df_day <- read.csv("FILEPATH/modeling/model_df_day.csv")
model_df_night <- read.csv("FILEPATH/modeling/model_df_night.csv")


model_day <- lm(t_air_offset ~ ENL1_mean, data = model_df_day)
summary(model_day)
plot(model_df_day$t_air_offset ~ model_df_day$ENL1_mean,
     ylim= c(-25, 15))
abline(model_day)

plot(model_day)

model_night <- lm(t_air_offset ~ ENL1_mean, data = model_df_night)
summary(model_night)
plot(model_df_night$t_air_offset ~ model_df_night$ENL1_mean,
     ylim= c(-25, 15))
abline(model_night)

plot(model_night)


#############
results_df <- read.csv("FILEPATH/Modeling/corrmat/results_df.csv")
unique(results_df[,c(1,2,33,39,77)])

##################################
##########CORRMAT#################
##################################
# remove columns (trim df for cormat)
results_corrmat <- dplyr::select(results_df, -c("las_name", "hourofday", 
                             "t_soil_min","t_soil_max","t_air_min","t_air_max",
                             #"t_air_mean", "t_air_med",
                             #"t_ERA5_mean", "t_ERA5_med",
                             "pad_n", "pad_min", "pad_sd", "pad_n",
                             "cc_class", "als_elevation",
                             "veg_type", "veg_class",
                             "RH98_mean", "RH98_med", "RH98_sd", "RH98_min", "RH98_max",
                             "RH75_mean", "RH75_med", "RH75_sd", "RH75_min", "RH75_max",
                             "CR_min", "FHD_min", "pai_min", "RH25_min", "RH50_min", "RH95_min",
                             "FHD2_mean", "FHD2_max", "FHD2_sd",
                             "ENL1_mean", "ENL1_max", "ENL1_sd",
                             "CR_sd", "RH25_sd", "pai_med", "pad_med", "ENL2_sd",
                             "pai_sd", "RH25_med", "RH50_med", "RH50_sd","RH95_med", "FHD_med",
                             "FHD_sd", "RH95_sd", "CR_med"))

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
                       RH25_mx = RH25_max,
                       RH50_mn = RH50_mean,
                       RH50_mx = RH50_max,
                       RH95_mn = RH95_mean,
                       RH95_mx = RH95_max,
                       ENL2_mn = ENL2_mean,
                       ENL2_mx = ENL2_max
)
res_var <- corr_renamed$air

corr_renamed <- select(corr_renamed, 
                          -c("sor", "offd", 
                             "offn", "pad_mn", "pad_mx"))

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
center_corrmat <- as.data.frame(center_scale(corr_renamed))


set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(air ~., data = corr_renamed,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 5)

##SET RESOLUTION VARIABLE AND PREDICTION VARIABLE
res_var <- corr_renamed$air
pred_var <- data.matrix(corr_renamed[, c('cc', 'elev', 'FHD_mn', 'FHD_mx',
                                         'CR_mn', 'CR_mx', 'pai_mn', 'pai_mx',
                                         'RH25_mn', 'RH25_mx', 'RH50_mn', 'RH50_mx',
                                         'RH95_mn', 'RH95_mx', 'ENL2_mn', "ENL2_mx")])

##LASSO REGRESSION 
cv_model <- cv.glmnet(pred_var, res_var, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
#produce plot of test MSE by lambda value
plot(cv_model) 
best_model <- glmnet(pred_var, res_var, alpha = 1, lambda = best_lambda)
coef(best_model)
best_model$dev.ratio

#MULTIPLE LINEAR REGRESSION
res_var <- corr_renamed$air
pred_var <- data.matrix(corr_renamed[, c('cc', 'elev', 'FHD_mn',
                                         'CR_mn', 'pai_mn',
                                         'RH25_mn', 'ENL2_mn')])

pred_var <- data.matrix(corr_renamed[, c('cc', 'elev', 'FHD_mn', 'FHD_mx',
                                         'CR_mn', 'CR_mx', 'pai_mn', 'pai_mx',
                                         'RH25_mn', 'RH25_mx', 'RH50_mn', 'RH50_mx',
                                         'RH95_mn', 'RH95_mx', 'ENL2_mn', "ENL2_mx")])
#MODEL WITH ALL VARIABLES
all_var_lm <- lm(formula = air ~ 
                   elev +
                   cc +
                   CR_mn +
                   CR_mx +
                   pai_mn +
                   pai_mx +
                   FHD_mn +
                   FHD_mx +
                   ENL2_mn +
                   ENL2_mx +
                   RH25_mn +
                   RH25_mx +
                   RH50_mn + 
                   RH50_mx + 
                   RH95_mn +
                   RH95_mx
                 , 
                 data = corr_renamed)
summary(all_var_lm)
vif(all_var_lm)

plot(corr_renamed$CR_mn ~ corr_renamed$pai_mn)


lm_cc <- lm(formula = offn_max ~  RH95_mean, data = results_df)
summary(lm_cc)
vif(lm_cc)
# MODEL WITH ONLY CC RS = 0.27 
lm_cc <- lm(formula = air ~  elev*cc + ENL2_mn + CR_mn*pai_mn, data = corr_renamed)
summary(lm_cc)
vif(lm_cc)

lm_cc <- lm(formula = air ~  elev*cc + ENL2_mn + CR_mn*pai_mn, data = corr_renamed)
lm_cc2 <- lm(formula = air ~  elev*cc + ENL2_mn + CR_mn*pai_mn + slope, data = corr_renamed)
lm_cc3 <- lm(formula = air ~  elev*cc + ENL2_mn + CR_mn*pai_mn + slope + aspect, data = corr_renamed)
stargazer(lm_cc,lm_cc2,lm_cc3, single.row = T, column.sep.width = "0.5pt", font.size = "scriptsize")

lm_cc2 <- lm(formula = air ~ elev + ENL2_mn + CR_mn + cc*elev, data = corr_renamed)
summary(lm_cc2)
lm_cc3 <- lm(formula = air ~ elev + ENL2_mn + CR_mn*pai_mn + cc*elev, data = corr_renamed)
summary(lm_cc3)
stargazer(lm_cc3,lm_cc2,lm_cc, single.row = T, column.sep.width = "0.5pt", font.size = "scriptsize")

lm_cc1 <- lm(formula = air ~ cc + elev, data = corr_renamed)
summary(lm_cc4)
lm_cc2 <- lm(formula = air ~ ENL2_mn + elev, data = corr_renamed)
summary(lm_cc4)
lm_cc3 <- lm(formula = air ~ FHD_mn + elev, data = corr_renamed)
summary(lm_cc4)
stargazer(lm_cc1, lm_cc2, lm_cc3, single.row = T)

# MODEL WITH ALL MEAN VALUES
lm_all_var <- lm(formula = res_var ~  cc + elev + CR_mn + FHD_mn + CR_mn+elev + pai_mn + RH25_mn + ENL2_mn, data = corr_renamed)
summary(lm_all_var)
vif(lm_all_var)
# RS = 0.5 but high colinearity (coeffs dont tell much)

#MODEL WITH ALL STRUCTURAL METRICS (NO CC, NO ELEV)
lm_struc <- lm(formula = res_var ~ CR_mn + FHD_mn +pai_mn + RH25_mn + ENL2_mn, data = corr_renamed)
summary(lm_struc)
vif(lm_struc)
# RS = 0.37 but high colinearity between FHD/ENL2/PAI, RH25 is statistically insignificant

#MODEL EXCLUDING RH25 (statistically insigifnicant)
lm_struc_norh <- lm(formula = res_var ~ CR_mn + FHD_mn +pai_mn  + ENL2_mn, data = corr_renamed)
summary(lm_struc_norh)
vif(lm_struc_norh)
# RH = 0.37 but high colinearity between FHD/ENL2

# MODEL USING PAI AND CR 
lm_pai_cr <-  lm(formula = res_var ~ elev + CR_mn + pai_mn, data = corr_renamed)
summary(lm_pai_cr)
vif(lm_pai_cr)
# RH 0.25, CR doesnt really add much, but with elevation R2 increases to 0.39

###ADDING CANOPY COVER TO THE MODEL DOESNT REALLY INCREASE R2, AFFECT COEFFICIENT (ITS NOT VERY SIGNIFICANT)
lm_cr_fhd <- lm(formula = res_var ~ elev + CR_mn + FHD_mn + FHD_mx, data = corr_renamed)
summary(lm_cr_fhd)
vif(lm_cr_fhd)
plot(lm_cr_fhd)
# mean-center and standardize variables
d2 <- data.frame(scale(corr_renamed))
lm_test <- lm(formula =  air ~ CR_mn*elev + pai_mn + elev + cc*elev, data = d2)
lm_test2 <- lm(formula = air ~ CR_mn*elev + FHD_mn + elev + cc*elev, data = d2)
lm_test3 <- lm(formula = air ~ CR_mn*elev + ENL2_mn + elev + cc*elev, data = d2)
stargazer(lm_test,lm_test2,lm_test3, single.row = T, font.size = "footnotesize")
lm_plot <- lm(formula = res_var ~  cc  + CR_mn + ENL2_mn + elev + cc*elev + ENL2_mn*elev, data = corr_renamed)
lm_plot2 <- lm(formula = sor ~  cc  + CR_mn + ENL2_mn + elev + cc*elev + ENL2_mn*elev, data = corr_renamed)

round(coef(lm_test), 2)
summary(lm_test)
vif(lm_test)

######################################
####INTERACTION PLOTS USING BASE R####
######################################
air_temp <- plot(effect(term="ENL2_mn:elev", mod=lm_plot, xlevels=8), multiline = T, colors=colorRampPalette(c("#A89100","white","#2D6993"))(8), lwd=2, main="Air Temperature", sub="",
                                                                                                          xlab="CC (%)", ylab="Daily Temperature Range (°C)")
air_temp
soil_temp <- plot(effect(term="cc:elev", mod=lm_plot2, xlevels=8), multiline = T, colors=colorRampPalette(c("#A89100","white","#2D6993"))(8), lwd=2, main="Soil Temperature", sub="",
             xlab="CC (%)", ylab="")

plot.new()
grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
soil_temp


fig1 <- ggarrange(air_temp, soil_temp,
                  labels = c("", ""),
                  ncol = 2, nrow = 1,
                  align = "hv"
)
fig1


######################################
####INTERACTION PLOTS USING GGPLOT####
######################################
#########################################################
####INTERACTION PLOTS OF CC/ELEV AND CR/PAI FOR RESULTS##
#########################################################
lm_plot <- lm(formula = res_var ~  cc  + ENL2_mn + elev + cc*elev + CR_mn*pai_mn, data = corr_renamed)
summary(lm_plot)
effect_ccair <- as.data.frame(effect(term="cc:elev", mod=lm_plot, xlevels=8))
ccair <- ggplot(effect_ccair) +
  aes(x = cc, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = cc, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "CC (%)", y = "", title = "")+
  scale_color_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  scale_fill_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")

lm_plot2 <- lm(formula = res_var ~  cc  + ENL2_mn + elev + cc*elev + CR_mn*pai_mn, data = corr_renamed)
effect_crpai <- as.data.frame(effect(term="CR_mn:pai_mn", mod=lm_plot2, xlevels=8))
crpai <- ggplot(effect_crpai) +
  aes(x = CR_mn, y = fit, color = factor(pai_mn), group = pai_mn) +
  geom_point(aes(x = CR_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(pai_mn)), alpha = 0.25, color = NA)+
  labs(x = "Mean CR", y = "", title = "")+
  scale_color_viridis_d(expression(atop(paste("PAI", " (",m^2,m^-2,")"),
                                        paste("and SE"))))+
  scale_fill_viridis_d(expression(atop(paste("PAI"," (",m^2,m^-2,")"),
                                       paste("and SE"))))+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")



fig1 <- ggarrange(ccair, crpai, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1,
                  align = "hv"
)

annotate_figure(
  fig1,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("Mean PAI", color = "black"),
  left = text_grob(bquote(""~DTR[a]~"- Daily air temperature range (°C)"),
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)
fig1

ggsave("FILEPATH/figures/interaction_plots/cc_elev_cr_pai.pdf",
       width = 20,
       height = 10,
       units = "cm")

######################################
####INTERACTION PLOTS OF CC AND ELEV##
######################################
lm_plot <- lm(formula = res_var ~  cc*elev, data = corr_renamed)
summary(lm_plot)
effect_ccair <- as.data.frame(effect(term="cc:elev", mod=lm_plot, xlevels=8))
ccair <- ggplot(effect_ccair) +
  aes(x = cc, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = cc, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "CC (%)", y = "Daily Temperature Range (°C)", title = "Air Temperature")+
  scale_color_viridis_d("Elevation\nm .a.s.l.")+
  scale_fill_viridis_d("Elevation\nm .a.s.l.")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
   theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")

legend <- get_legend(ccair)
ccair <- ggplot(effect_ccair) +
  aes(x = cc, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = cc, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "CC (%)", y = "Daily Temperature Range (°C)", title = "Air Temperature")+
  scale_color_viridis_d("Elevation\nm .a.s.l.")+
  scale_fill_viridis_d("Elevation\nm .a.s.l.")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  theme_bw()

lm_plot2 <- lm(formula = sor ~  cc  + CR_mn + ENL2_mn + elev + cc*elev + CR_mn*pai_mn, data = corr_renamed)
effect_ccsoil <- as.data.frame(effect(term="cc:elev", mod=lm_plot2, xlevels=8))
ccsoil <- ggplot(effect_ccsoil) +
  aes(x = cc, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = cc, y = sor), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), alpha = 0.25, color = NA)+
  labs(x = "CC (%)", y = "Daily Temperature Range (°C)", title = "Soil Temperature")+
  scale_color_viridis_d("Elevation\nm .a.s.l.")+
  scale_fill_viridis_d("Elevation\nm .a.s.l.")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  theme_bw()

fig1 <- ggarrange(ccair, ccsoil, 
                  labels = c("", ""),
                  ncol = 2, nrow = 1,
                  align = "hv",
                  common.legend = T,
                  legend.grob = legend,
                  legend = "bottom"
)
fig1

ggsave("FILEPATH/figures/interaction_plots/cc_elev_complex.png",
       width = 20,
       height = 10,
       units = "cm")
########################################
####INTERACTION PLOTS OF ENL2 AND ELEV##
########################################
lm_plot <- lm(formula = res_var ~  cc  + ENL2_mn + elev + cc*elev + CR_mn*pai_mn, data = corr_renamed)
summary(lm_plot)
effect_ccair <- as.data.frame(effect(term="cc:elev", mod=lm_plot, xlevels=8))
ccair <- ggplot(effect_ccair) +
  aes(x = cc, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = cc, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "CC (%)", y = "", title = "")+
  scale_color_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  scale_fill_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")

lm_plot2 <- lm(formula = res_var ~  cc  + ENL2_mn + elev + cc*elev + CR_mn*pai_mn, data = corr_renamed)
effect_crpai <- as.data.frame(effect(term="CR_mn:pai_mn", mod=lm_plot2, xlevels=8))
crpai <- ggplot(effect_crpai) +
  aes(x = CR_mn, y = fit, color = factor(pai_mn), group = pai_mn) +
  geom_point(aes(x = CR_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(pai_mn)), alpha = 0.25, color = NA)+
  labs(x = "Mean CR", y = "", title = "")+
  scale_color_viridis_d(expression(atop(paste("PAI", " (",m^2,m^-2,")"),
                                        paste("and SE"))))+
  scale_fill_viridis_d(expression(atop(paste("PAI"," (",m^2,m^-2,")"),
                                       paste("and SE"))))+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")



lm_plot <- lm(formula = res_var ~  ENL2_mn*elev + cc*elev + CR_mn*pai_mn+ slope, data = corr_renamed)
effect_ENLair <- as.data.frame(effect(term="ENL2_mn:elev", mod=lm_plot, xlevels=8))
ENLair <- ggplot(effect_ENLair) +
  aes(x = ENL2_mn, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = ENL2_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "Mean ENL2", y = "", title = "")+
  scale_color_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  scale_fill_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")
ENLair
legend <- get_legend(ENLair)
ENLair <- ggplot(effect_ENLair) +
  aes(x = ENL2_mn, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = ENL2_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "Mean ENL2", y = "", title = "")+
  scale_color_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  scale_fill_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  theme_bw()

lm_plot2 <- lm(formula = air ~ FHD_mn + FHD_mn*elev + cc + elev + slope , data = corr_renamed)
summary(lm_plot2)
effect_ENLsoil <- as.data.frame(effect(term="FHD_mn:elev", mod=lm_plot2, xlevels=8))
ENLsoil <- ggplot(effect_ENLsoil) +
  aes(x = FHD_mn, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = FHD_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), alpha = 0.25, color = NA)+
  labs(x = "Mean FHD", y = "", title = "")+
  scale_color_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  scale_fill_viridis_d("Elevation\n(m.a.s.l.)\nand SE")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  theme_bw()
ENLsoil

fig1 <- ggarrange(ccair, crpai, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1,
                  align = "hv"
)
fig1
fig2 <- ggarrange(ENLair, ENLsoil, 
                  labels = c("C", "D"),
                  ncol = 2, nrow = 1,
                  align = "hv",
                  common.legend = T,
                  legend.grob = legend,
                  legend = "bottom"
)
fig2
fig3 <- ggarrange(fig1, fig2, 
                  labels = c("", ""),
                  ncol = 1, nrow = 2,
                  align = "hv"
)
fig3
annotate_figure(
  fig3,
  top = text_grob("", color = "black", face = "bold", size = 14),
  #bottom = text_grob("Mean PAI", color = "black"),
  left = text_grob(bquote(""~DTR[a]~"- Daily air temperature range (°C)"),
                   color = "black", rot = 90),
  #right = text_grob("Contains modified Copernicus Climate Change Service Information [2019]",
  #size = 8, rot = 270, face = "italic")
  # fig.lab = "Figure 1", fig.lab.face = "bold"
)
fig1

ggsave("FILEPATH/figures/interaction_plots/interactions.pdf",
       width = 20,
       height = 20,
       units = "cm")

ggsave("FILEPATH/figures/interaction_plots/ENL_FHD_elev_complex_v2.pdf",
       width = 20,
       height = 10,
       units = "cm")

########################################
####INTERACTION PLOTS OF FHD AND ELEV##
########################################
lm_plot <- lm(formula = air  ~ cc + FHD_mn + CR_mn + elev + FHD_mn*elev, data = corr_renamed)
summary(lm_plot)
effect_FHDair <- as.data.frame(effect(term="FHD_mn:elev", mod=lm_plot, xlevels=8))
FHDair <- ggplot(effect_FHDair) +
  aes(x = FHD_mn, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = FHD_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "FHD", y = "Daily Temperature Range (°C)", title = "Air Temperature")+
  scale_color_viridis_d("Elevation\nm .a.s.l.")+
  scale_fill_viridis_d("Elevation\nm .a.s.l.")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "bottom")
FHDair
legend <- get_legend(FHDair)
FHDair <- ggplot(effect_FHDair) +
  aes(x = FHD_mn, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = FHD_mn, y = air), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), color = NA, alpha = 0.25)+
  labs(x = "FHD", y = "Daily Temperature Range (°C)", title = "Air Temperature")+
  scale_color_viridis_d("Elevation\nm .a.s.l.")+
  scale_fill_viridis_d("Elevation\nm .a.s.l.")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme_bw()+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")

lm_plot2 <- lm(formula = sor  ~ cc + FHD_mn + CR_mn + elev + cc*elev + FHD_mn*elev, data = corr_renamed)
effect_FHDsoil <- as.data.frame(effect(term="FHD_mn:elev", mod=lm_plot2, xlevels=8))
FHDsoil <- ggplot(effect_FHDsoil) +
  aes(x = FHD_mn, y = fit, color = factor(elev), group = elev) +
  geom_point(aes(x = FHD_mn, y = sor), data = corr_renamed, color = "gray", alpha = 0.2)+
  geom_line(size = 0.5)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(elev)), alpha = 0.25, color = NA)+
  labs(x = "FHD", y = "Daily Temperature Range (°C)", title = "Soil Temperature")+
  scale_color_viridis_d("Elevation\nm .a.s.l.")+
  scale_fill_viridis_d("Elevation\nm .a.s.l.")+
  ylim(c(-1,30))+
  # scale_color_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  # scale_fill_manual(values = colorRampPalette(c("#A89100","white","#2D6993"))(8))+
  theme(axis.text = element_text(face="bold"),
        legend.position = "none")+
  theme_bw()

fig1 <- ggarrange(FHDair, FHDsoil, 
                  labels = c("", ""),
                  ncol = 2, nrow = 1,
                  align = "hv",
                  common.legend = T,
                  legend.grob = legend,
                  legend = "bottom"
)
fig1

ggsave("FILEPATH/figures/interaction_plots/FHD_elev_complex.pdf",
       width = 20,
       height = 10,
       units = "cm")

plot(x = corr_renamed$FHD_mn, y = corr_renamed$cc)
#########################
####TAKEAWAY FOR TODAY:
#THE INTERACTION PLOTS CHANGE STRONGLY IN MORE COMPLEX MODELS VERSUS SIMPLE LINEAR MODELS WITH ELEVATION
#AL DIFFERENCES? SEE CHAT WITH IRIS
#ADDING AN INTERACTION TERM FOR A SECOND PREDICTOR POSSIBLE?
#WHAT DOES THAT MEAN FOR THE MODELS?



###########OFFSET
lm_cc <- lm(formula = offd_min ~ ENL2_mn + elev, data = corr_renamed)
summary(lm_cc)
vif(lm_cc)
