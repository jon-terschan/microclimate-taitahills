# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
require(dplyr)

###########################################################
######ELEVATION OF MICROCLIMATE/RESEARCH STATION###########
###########################################################
stats_plus_micro <- read.csv("FILEPATH/model_df_2_1.csv")
elev <- unique(stats_plus_micro[,c(1, 17)])
las_name <- c("taita_research_station", "kishenyi_station", "maktau_station")
gps_elevation <- c(1415, 1550, 1005)
df <- data.frame(las_name, gps_elevation)
elev <- rbind(elev, df)

###########################################################
#####CONVERT ERA5 DATA TO POSIXC AND ADD MEAN ELEVATION####
###########################################################
# THIS IS REQUIRED BECAUSE R AUTOMATICALLY READS IN DATES
# IN THE WRONG FORMAT (NOT POSICTX BUT DATE)
# DEFINE FUNCTION TO AUTOMATE PROCESS
timestamp_converter <- function(tile_name){
  tile <- read.csv(paste("FILEPATH/tile_dfs_as_datetime/", tile_name, "_AT2M_datetime.csv", sep=""))
  date <- tile$date
  hour <- tile$hr
  date_string <- paste(tile$date, paste(paste(hour,"00","00",sep=":"),sep="."),sep = " ")
  tile$TIMESTAMP <- as.POSIXct(date_string,tz="UTC",format="%Y-%m-%d %H:%M:%OS")
  tile <- tile[,c(1,4)]
  elev <- read.csv(paste("FILEPATH/ERA5_mean_elevation/", tile_name, ".csv", sep=""))
  tile$tile_elevation <- elev[,2]
  write.csv(tile,paste("FILEPATH/tile_dfs_posix_timestamp/",tile_name,"_AT2M_TIMESTAMP.csv", sep = ""), row.names = FALSE)
}
# ENTER TILENAME AS CHARACTER TO AUTOMATE PROCESS
#timestamp_converter("tile45")

####################################################################
#####CORRECT DAYTIME/NIGHTTIME ACCORDING TO LAPSE RATES ############
####################################################################
tile_name <- "tile32"
las_file <- "las_11"
tile <- read.csv(paste("E:/Macroclimate/tile_dfs_posix_timestamp/",tile_name,"_AT2M_TIMESTAMP.csv", sep = ""))

# CORE IDEA BEHIND THE NESTED FUNCTION
#tile$micro_elevation <- elev[elev$las_name == "las_11", 2]
#tile$AT2m_corrected_2 <- tile$AT2m + ((tile$tile_elevation - tile$micro_elevation) * -0.0092)

# EXTRACT DAYTIME AND NIGHTTIME FUNCTIONS ADAPTED TO THIS PARTICULAR SITUATION
extract_daytime <- function(df) {
  df[,2] <- as.POSIXct(df[,2], tz = "UTC")
  df$hms <- hms::as_hms(df[,2])
  df_daytime <- df %>% select(TIMESTAMP, AT2m, hms, tile_elevation) %>% 
    filter(hms >= hms::as_hms('05:59:00'),
           hms <= hms::as_hms('18:01:00'))
  return(df_daytime)
}

extract_nighttime<- function(df){
  df[,2] <- as.POSIXct(df[,2], tz = "UTC")
  df$hms <- hms::as_hms(df[,2])
  df_nighttime1 <- df %>% select(TIMESTAMP, AT2m, hms, tile_elevation) %>% 
    filter(hms >= hms::as_hms('18:01:00'))
  df_nighttime2 <- df %>% select(TIMESTAMP, AT2m, hms, tile_elevation) %>% 
    filter(hms <= hms::as_hms('05:59:00'))
  df_nighttime_bind <- rbind(df_nighttime1, df_nighttime2)
  df_nighttime <- df_nighttime_bind %>% select(TIMESTAMP, AT2m, hms, tile_elevation) %>% 
    arrange(df_nighttime_bind$TIMESTAMP)
  return(df_nighttime)
}

# NESTED FUNCTION TO CORRECT FOR LAPSE RATES, LAS_NAME MUST BE CHARACTER
# AND THE LAPSE RATES ITSELF MUST BE SET MANUALLY FROM THE LINEAR MODEL 
# COEFFICIENTS (SEE LAPSE RATE CALCULATION)
# THE IDEA IS TO TAKE THE 
correct_lapse_rate <- function(tile, las_file){
  # THIS PART IS JUST START OF THIS SCRIPT TO TAKE CARE OF DEPENDENCIES
  # FIRST DEPENDENCY IS THE OVERVIEW DATAFRAME WHICH IS STRUCTURAL TRAITS +
  # MICROCLIMATE DATA FOR ALL SITES
  stats_plus_micro <- read.csv("FILEPATH/stats_plus_micro_v2.csv")
  elev <- unique(stats_plus_micro[,c(2, 27)]) # MAY CHANGE ACCORDING TO FORMAT
  las_name <- c("taita_research_station", "kishenyi_station", "maktau_station")
  gps_elevation <- c(1395, 1550, 1005)
  df <- data.frame(las_name, gps_elevation)
  elev <- rbind(elev, df)
  # EXTRACT DAYTIME
  tile_daytime <- extract_daytime(tile)
  # ADD ELEVATION OF MICROCLIMATE PLOT
  tile_daytime$micro_elevation <- elev[elev$las_name == as.character(paste(las_file, sep = "")), 2]
  # CORRECT TEMPERATURE BASED ON DAYTIME LAPSE RATE
  tile_daytime$AT2m_corrected <- tile_daytime$AT2m + 
                                ((tile_daytime$tile_elevation - tile_daytime$micro_elevation) 
                                * -0.0092)
  # SAME THING FOR NIGHT
  tile_nighttime <- extract_nighttime(tile)
  tile_nighttime$micro_elevation <- elev[elev$las_name == as.character(paste(las_file, sep = "")), 2]
  tile_nighttime$AT2m_corrected <- tile_nighttime$AT2m + 
                                ((tile_nighttime$tile_elevation - tile_nighttime$micro_elevation) 
                                * -0.0059)
  # COMBINE DAY AND NIGHT AND ARRANGE THEM
  tile <- rbind(tile_daytime, tile_nighttime)
  tile <- tile %>% select(TIMESTAMP, AT2m_corrected, AT2m, tile_elevation, micro_elevation) %>% 
    arrange(tile$TIMESTAMP)
  return(tile)
}

# DOUBLE NESTED FUNCTION TO EXECUTE WORKFLOW 
execute_lapse_correction <- function(tile_name, las_file){
  tile <- read.csv(paste("E:/Macroclimate/tile_dfs_posix_timestamp/",tile_name,"_AT2M_TIMESTAMP.csv", sep = ""))
  tile_corrected <- correct_lapse_rate(tile, las_file)
  write.csv(tile_corrected, paste("E:/Macroclimate/ERA5_corrected_temperature/",las_file,"_AT2M_corrected.csv", sep = ""), row.names = FALSE)
}

# CHANGE TILE NAME AND LAS FILE PARAMETERS AND EXECUTE FUNCTION
tile_name <- "tile45"
las_file <- "las_15"
execute_lapse_correction(tile_name, las_file)




