# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
library(lubridate)
library(dplyr)
library(tidyverse)

# LOAD WEATHER STATION DATA
taita_research_station <- read.csv("FILEPATH/TaitaHillsRS_trimmed.csv")
kishenyi_station <- read.csv("FILEPATH/Kishenyi_trimmed.csv")
maktau_station <- read.csv("FILEPATH/Maktau_trimmed.csv")

# RENAME COLUMN 1 TO TIMESTAMP
taita_research_station <- rename(taita_research_station, TIMESTAMP = X)
# UNIQUE NAMES FOR COLUMNS
taita_research_station <- rename(taita_research_station, AirTC_Avg_Taita = AirTC_Avg)
kishenyi_station <- rename(kishenyi_station, AirTC_Avg_Kish = AirTC_Avg)
maktau_station <- rename(maktau_station, AirTC_Avg_Mak = AirTC_Avg)
# ADD ELEVATION IN UNIQUE COLUMNS
taita_research_station$taita_elev_m <- 1395
kishenyi_station$kish_elev_m <- 1550
maktau_station$mak_elev_m <- 1005

# MERGE TO A LAPSE DF
lapse <- merge(kishenyi_station, maktau_station, by = "TIMESTAMP")
lapse <- merge(lapse, taita_research_station, by = "TIMESTAMP")
# CALCULATE LAPSE RATE BASED ON DIFFERENT STATIONS
lapse$lapse_Ckm_Kish_Mak <- (lapse$AirTC_Avg_Kish - lapse$AirTC_Avg_Mak)/(lapse$kish_elev_m - lapse$mak_elev_m)
lapse$lapse_Ckm_Kish_Tai <- (lapse$AirTC_Avg_Kish - lapse$AirTC_Avg_Taita)/(lapse$kish_elev_m - lapse$taita_elev_m)
lapse$lapse_Ckm_Tai_Mak <- (lapse$AirTC_Avg_Taita - lapse$AirTC_Avg_Mak)/(lapse$taita_elev_m - lapse$mak_elev_m)
lapse$lapse_mean <- rowMeans(lapse[,c('lapse_Ckm_Kish_Mak', 'lapse_Ckm_Kish_Tai', 'lapse_Ckm_Tai_Mak')], na.rm=TRUE)
# MANUALLY CALCULATED LAPSE RATE
summary(lapse)

# CHECK PEARSONS CORELLATION COEFFICIENT 
cor(lapse$AirTC_Avg_Kish, lapse$AirTC_Avg_Mak)
cor(lapse$AirTC_Avg_Kish, lapse$AirTC_Avg_Taita, use = "complete.obs")
# CHECK NA
which(is.na(lapse$AirTC_Avg_Taita))
# 1 NA IN TAITA DATASET
# RENAME 1 COLUMN IN TAITA
taita_research_station <- rename(taita_research_station, TIMESTAMP = X)

# ASSIGN ELEVATION TO NEW COLUMN
taita_research_station$elev_m <- 1415
kishenyi_station$elev_m <- 1550
maktau_station$elev_m <- 1005
# APPEND TO SINGLE DF
lapse_append <- rbind(taita_research_station, kishenyi_station)
lapse_append <- rbind(lapse_append, maktau_station)
# GET RID OF NA
lapse_append <- lapse_append[-which(is.na(lapse_append$AirTC_Avg)), ]

# LINEAR MODEL TO FIND LAPSE RATE
lms <- lm(AirTC_Avg ~ elev_m, data = lapse_append) 
# EXTRACT LAPSE RATE
coef(lms)
summary(lms)

########################################
#####NIGHT AND DAYTIME LAPSERATES#######

# RENAME FIRST COLUMN IN TAITA DATA:
taita_research_station <- rename(taita_research_station, TIMESTAMP = X)
# CONVERT TO POSIXCT BECAUSE POXICT GETS LOST WHEN EXPORTING 
maktau_station[,1] <- as.POSIXct(maktau_station[,1], tz = "UTC")
# NEW COLUMN WITH HOURS MINUTES SECONDS (HMS)
maktau_station$hms <- hms::as_hms(maktau_station[,1])

# SELECT DATA - DAYTIME FROM 6AM TO 6PM
maktau_daytime <- maktau_station %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
  filter(hms >= hms::as_hms('05:59:00'),
         hms <= hms::as_hms('18:01:00'))
# THIS SHOULD BE ONE STEP BUT I DIDNT MANAGE TO CONVERGE 
# SAME STEP BUT FOR NIGHTTIME TEMPS
maktau_nighttime1 <- maktau_station %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
  filter(hms >= hms::as_hms('18:01:00'))
maktau_nighttime2 <- maktau_station %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
  filter(hms <= hms::as_hms('05:59:00'))
# APPEND THEM
maktau_nighttime_bind <- rbind(maktau_nighttime1, maktau_nighttime2)
# ORDER THEM BY TIMESTAMP 
maktau_nighttime <- maktau_nighttime_bind %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
  arrange(maktau_nighttime_bind$TIMESTAMP)

# WRITE TWO FUNCTIONS FOR BOTH STEPS
extract_daytime <- function(df) {
  df[,1] <- as.POSIXct(df[,1], tz = "UTC")
  df$hms <- hms::as_hms(df[,1])
  df_daytime <- df %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
    filter(hms >= hms::as_hms('05:59:00'),
           hms <= hms::as_hms('18:01:00'))
  return(df_daytime)
}

extract_nighttime<- function(df){
  df[,1] <- as.POSIXct(df[,1], tz = "UTC")
  df$hms <- hms::as_hms(df[,1])
  df_nighttime1 <- df %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
    filter(hms >= hms::as_hms('18:01:00'))
  df_nighttime2 <- df %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
    filter(hms <= hms::as_hms('05:59:00'))
  df_nighttime_bind <- rbind(df_nighttime1, df_nighttime2)
  df_nighttime <- df_nighttime_bind %>% select(TIMESTAMP, AirTC_Avg, hms) %>% 
    arrange(df_nighttime_bind$TIMESTAMP)
  return(df_nighttime)
}

# APPLY TO DATA SETS AND ADD ELEVATION
maktau_day <- extract_daytime(maktau_station)
maktau_day$elev <- 1005
maktau_night <- extract_nighttime(maktau_station)
maktau_night$elev <- 1005
kish_day <- extract_daytime(kishenyi_station)
kish_day$elev <- 1550
kish_night <- extract_nighttime(kishenyi_station)
kish_night$elev <- 1550
taita_day <- extract_daytime(taita_research_station)
taita_day$elev <- 1415
taita_night <- extract_nighttime(taita_research_station)
taita_night$elev <- 1415

all_night <- rbind(kish_night, taita_night, maktau_night)
all_day <- rbind(kish_day, taita_day, maktau_day)

# REMOVE MISSING ENTRIES
all_day <- all_day[-which(is.na(all_day$AirTC_Avg)), ]

# LINEAR REGRESSION AND LAPSE RATE EXTRACTION
night_lapse <- lm(AirTC_Avg ~ elev, data = all_night) 
day_lapse <- lm(AirTC_Avg ~ elev, data = all_day) 
coef(night_lapse)
summary(night_lapse)
coef(day_lapse)
summary(day_lapse)

###################################################################
#####NIGHT AND DAYTIME LAPSERATES BASED ON MICROCLIMATE DATA#######
#####ONLY NECESSARY FOR DIRECT COMPARISONS OF MICROCLIMATE TEMP###
###################################################################

# READ IN MICROCLIMATE DF
stats_plus_micro <- read.csv("FILEPATH/stats_plus_micro_v2.csv")
# LAPSE RATE BASED ON DAILY SUMMARY (MEANS)
lapse_daily_micro <- lm(tmean_air ~ gps_elevation, data = stats_plus_micro)
coef(lapse_daily_micro)

# READ IN HOURLY MEAN AIR TEMP DF
hourly_micro <- read.csv("FILEPATH/all_plots_hourly_summary.csv")
# RENAME DF ACCORDING TO EARLIER NAMING CONVENTION
hourly_micro <- hourly_micro %>% 
  rename(las_name = plot,
         TIMESTAMP = hourofday,
         AirTC_Avg = t_air)
# SUBSET ELEVATION DATA FROM OVERVIEW DF
elevation_data <- stats_plus_micro %>%
  select(las_name, gps_elevation, als_elevation, aspect, slope)
# JOIN ELEVATION DATA AND HOURLY OBSERVATIONS
lapse_hourly_micro <- left_join(hourly_micro, unique(elevation_data), by="las_name")
# UNNECESSARY COLUMN I FORGOT TO DELETE; REMOVE NOW
lapse_hourly_micro <- lapse_hourly_micro[,-1]

# EXTRACT DAYTIME: ESSENTIALLY ABOVE FUNCTION BUT INCLUDES MORE PARAMETERS IN THE
# DPLYR SELECT
lapse_hourly_micro[,1] <- as.POSIXct(lapse_hourly_micro[,1], tz = "UTC")
lapse_hourly_micro$hms <- hms::as_hms(lapse_hourly_micro[,1])
hourly_daytime <- lapse_hourly_micro %>% select(TIMESTAMP, AirTC_Avg, hms, las_name, gps_elevation, aspect, slope) %>% 
    filter(hms >= hms::as_hms('05:59:00'),
           hms <= hms::as_hms('18:01:00'))
# DAYTIME LAPSE RATE
lapse_hourly_day <- lm(AirTC_Avg ~ gps_elevation, data = hourly_daytime)
coef(lapse_hourly_day)

# EXTRACT NIGHTTIME 
lapse_hourly_micro[,1] <- as.POSIXct(lapse_hourly_micro[,1], tz = "UTC")
lapse_hourly_micro$hms <- hms::as_hms(lapse_hourly_micro[,1])
df_nighttime1 <- lapse_hourly_micro %>% select(TIMESTAMP, AirTC_Avg, hms, las_name, gps_elevation, aspect, slope) %>% 
    filter(hms >= hms::as_hms('18:01:00'))
df_nighttime2 <- lapse_hourly_micro %>% select(TIMESTAMP, AirTC_Avg, hms, las_name, gps_elevation, aspect, slope) %>% 
    filter(hms <= hms::as_hms('05:59:00'))
df_nighttime_bind <- rbind(df_nighttime1, df_nighttime2)
hourly_nighttime <- df_nighttime_bind %>% select(TIMESTAMP, AirTC_Avg, hms, las_name, gps_elevation, aspect, slope) %>% 
    arrange(df_nighttime_bind$TIMESTAMP)
# NIGHTTIME LAPSE RATE
lapse_hourly_night <- lm(AirTC_Avg ~ gps_elevation, data = hourly_nighttime )
coef(lapse_hourly_night)

