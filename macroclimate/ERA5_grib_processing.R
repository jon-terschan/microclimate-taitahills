# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
library("ecmwfr") # DOWNLOAD ERA5 FILES OVER CDS API
library("stringr") # TO CONCATENATE/EXTRACT STRINGS
library("raster") # RGDAL RASTER DRIVER
library("rNOMADS") # GRIBINFO
library("dplyr") # DATA HANDLING, SUMMARIZE
library("expm") # COMPUTATION OF MATRIX EXPONENTIALS ETC

# GET AND SET WORKING DIRECTORY
getwd()
setwd("E://")

# SET CDS USER DATA: 
# REFER TO https://cds.climate.copernicus.eu/api-how-to
# THIS IS AN R IMPLEMENTATION OF PYTHON CODE
wf_set_key(user = "USER ID",
           key = "API KEY",
           service = "cds")
# FORMULATE API REQUEST LIST 
# API REQUEST CAN BE GENERATED ON COPERNICUS DATA STORE 
# BUT HAS TO BE MANUALLY ADAPTED FROM PYTHON TO R CODE USING THE FOLLOWING
# TEMPLATE
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
request <- list("dataset_short_name" = "reanalysis-era5-land",
                "product_type" = "reanalysis",
                "variable" = "2m_temperature",
                "year" = c("2000", "2021"),
                'month'= c(
                  '01', '02', '03',
                  '04', '05', '06',
                  '07', '08', '09',
                  '10', '11', '12'
                ),
                'day'= c(
                  '01', '02', '03',
                  '04', '05', '06',
                  '07', '08', '09',
                  '10', '11', '12',
                  '13', '14', '15',
                  '16', '17', '18',
                  '19', '20', '21',
                  '22', '23', '24',
                  '25', '26', '27',
                  '28', '29', '30',
                  '31'
                ),
                "time" = c(
                  '00:00', '01:00', '02:00',
                  '03:00', '04:00', '05:00',
                  '06:00', '07:00', '08:00',
                  '09:00', '10:00', '11:00',
                  '12:00', '13:00', '14:00',
                  '15:00', '16:00', '17:00',
                  '18:00', '19:00', '20:00',
                  '21:00', '22:00', '23:00'
                  ),
                "area" = c(
                  -3.2, 38, -3.6,
                  38.7
                ),
                "format" = "grib",
                "target" = "era5-demo.grib")

# REQUEST FILE DOWNLOAD OVER COPERNICUS API, CHECK WD FIRST
file <- wf_request(user     = "USER ID",   # user ID (for authentification)
                   request  = request,  # the request
                   transfer = TRUE,     # download the file
                   path     = "FILEPATH")

# ALTERNATIVELY, THE FILES CAN BE MANUALLY REQUESTED AND DOWNLOADED VIA
# THE COPERNICUS DATA STORE

# USE WGRIB/RNOMADS TO RETRIEVE GRIBINFO
# NOW HERE COMES A FUN PART:
# RNOMADS MERELY WRAPS AN EXTERNAL FILE READER CALLED WGRIB1 OR WGRIB2
# DEPENDING ON WHICH FILE TYPE YOU USE
# THAT MEANS YOU NEED EITHER INSTALLED:
# WGRIB1: https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html
# WGRIB2: https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/ 
# HOWEVER WGRIB1 IS DEPRECATED AND THE DOWNLOAD LINKS NO LONGER WORK
# BUT IT IS POSSIBLE TO STILL DOWNLOAD THE CORRECT FILES BY USING THE LINK
# TO THE SOURCE CODE ON THE WGRIB2 PAGE AND MOVING THROUGH PARENT DIRECTORIES
# UNTIL ONE FINDS THE WGRIB.TAR.GZ. HERES THE LINK TO SAVE TIME
# https://www.ftp.cpc.ncep.noaa.gov/wd51we/wgrib/
# THE APPLICATION MUST STILL BE COMPILED, REFERENCE CAN BE FOUND ON THE INTERNET
# AND ON PROVIDED URLS

info <- GribInfo("FILEPATH/GRIBFILE.grib", file.type = "grib1")
# CONVERT TO DF
info <- as.data.frame(info$inventory)

# BRICK GRIB FILE USING RGDAL DRIVERS, RETURNS AN OBSCURE WARNING MESSAGE BUT
# WHATEVER?
GRIB <- brick("FILEPATH/ERA5_2MT_2019_2020.grib") 
# CONVERT RASTER BRICK INTO ARRAY OF VALUES
GRIB <- as.array(GRIB)
# SHOW SINGLE DATE IN ARRAY
GRIB[, , 17544]
#CREATE_TILE_DF: A FUNCTION TO: 
  # 1. EXTRACT DESIRED TILE FROM GRIB ARRAY
  # 2. ADD GRIBINFO STRING AS SECOND COLUMN
  # 3. EXTRACT DATE PART FROM GRIBINFO STRING
  # 4. CONVERT TEMP FROM KELVIN TO CELSIUS
  # 5. RENAME DATA COLUMN TO AT2m = AIR TEMPERATURE IN 2 M HEIGHT
  # 6. RETURN TILE
create_tile_df <- function(i, j){
  tile <- as.data.frame(GRIB[i,j,])
  tile["date"] <- info
  tile["hr"] <- str_extract(tile$date, "\\d+[h][r]")
  tile["date"] <- str_extract(tile$date, "\\d{2}\\d{2}\\d{2}\\d{2}")
  kelvin_to_c <- function(k){
    k-273.15
  }
  names(tile)[1] <- "AT2m"
  tile["AT2m"] <- kelvin_to_c(tile$AT2m)
  return(tile)
}

# ESSENTIALLY THE SAME BUT STEP FOR STEP INSTEAD OF PUT INTO A FUNCTION
#tile32 <- as.data.frame(GRIB[3,2,])
#tile32["date"] <- info
#tile32["hrs"] <- str_extract(tile32$date, "\\d+[h][r]")
#tile32["date"] <- str_extract(tile32$date, "\\d{2}\\d{2}\\d{2}")
#kelvin_to_c <- function(k){
#k-273.15
#}
#names(tile32)[1] <- "AT2m"
#tile32["AT2m"] <- kelvin_to_c(tile32$AT2m)

# TO APPLY THE FUNCTION CORRECTLY USER MUST FIND OUT WHICH TILES COVER
# THE AREA OF INTEREST MANUALLY WITH THE GEOLOCATIONS PROVIDED BY
# THE GRIB DOWNLOAD
# CREATE AND SAVE MY TILES
tile32 <- create_tile_df(3,2)
write.csv(tile32,"FILEPATH/tile32_AT2M_unsorted.csv", row.names = FALSE)
tile43 <- create_tile_df(4,3)
write.csv(tile43,"FILEPATH/tile43_AT2M_unsorted.csv", row.names = FALSE)
tile34 <- create_tile_df(3,4)
write.csv(tile34,"FILEPATH/tile34_AT2M_unsorted.csv", row.names = FALSE)
tile35 <- create_tile_df(3,5)
write.csv(tile35,"FILEPATH/tile35_AT2M_unsorted.csv", row.names = FALSE)
tile44 <- create_tile_df(4,4)
write.csv(tile44,"FILEPATH/tile44_AT2M_unsorted.csv", row.names = FALSE)
tile45 <- create_tile_df(4,5)
write.csv(tile45,"FILEPATH/tile45_AT2M_unsorted.csv", row.names = FALSE)

# CREATE FIX_DATETIME: A FUNCTION THAT CHANGES NUMERIC DATES INTO POSIXct DATETIME
# DEBUGGED FOR THE AIR TEMPERATURE DATASET WHICH IS FORMATED DIFFERENTLY THAN
# SKIN TEMPERATURE ODDLY ENOUGH
# I ASSUME OTHER VARIABLES WILL BE IN A DIFFERENT FORMAT AS WELL
fix_datetime <- function(tile){
  # 1. COPIES LAST TWOCHARACTERS OF DATE (HOURS) AND PUTS THEM INTO A DIFFERENT C
  # tile$hour <- substring(tile$date, nchar(tile$date) - 1, nchar(tile$date))
  # 2. GETS RID OF HOURS IN DATE COLUMN
  tile$date <- substring(tile$date, 1,6)
  # 2.2 GETS RID OF HR SYMBOL IN HR COLUMN (DEBUGGED)
  tile$hr <- substring(tile$hr, 1, nchar(tile$hr)-2)
  # 3. ADDS 20 TO YEAR EG 2019
  year = 20
  tile$date <- paste(year, tile$date, sep ="")
  # 4. NESTED FUNCTION TO INSERT SYMBOLS AT CUSTOM POSITION (DEBUGGED)
  fun_insert <- function(x, pos, insert) {       
    gsub(paste0("^(.{", pos, "})(.*)$"),
        paste0("\\1", insert, "\\2"),
        x)
  }
  # 5. APPLIES NESTED FUNCTION TO INSERT SLASHES (DEBUGGED)
  tile$date <- fun_insert(x = tile$date,    
                          pos = 4, 
                          insert = "-")
  tile$date <- fun_insert(x = tile$date,    
                          pos = 7, 
                          insert = "-")
  # 6. CONVERTS DATE INTO POSIXct DATETIME VARIABLE
  tile$date <- as.POSIXct(tile$date, format="%Y-%m-%d", tz="UTC")
  # 7. CONVERTS HOUR INTO NUMERIC VARIABLE
  tile$hr <- as.numeric(tile$hr)
  # 8. SUBSETS INTO STUDY TIME 17.05 - 05.02
  tile_subset <- tile[3266:9625,]
return(tile_subset)
}

# APPLY TO TILES AND SAVE THEM
tile32 <- fix_datetime(tile32)
write.csv(tile32,"FILEPATH/tile32_AT2M_datetime.csv", row.names = FALSE)
tile34 <- fix_datetime(tile34)
write.csv(tile34,"FILEPATH/tile34_AT2M_datetime.csv", row.names = FALSE)
tile35 <- fix_datetime(tile35)
write.csv(tile35,"FILEPATH/tile35_AT2M_datetime.csv", row.names = FALSE)
tile43 <- fix_datetime(tile43)
write.csv(tile43,"FILEPATH/tile43_AT2M_datetime.csv", row.names = FALSE)
tile44 <- fix_datetime(tile44)
write.csv(tile44,"FILEPATH/tile44_AT2M_datetime.csv", row.names = FALSE)
tile45 <- fix_datetime(tile45)
write.csv(tile45,"FILEPATH/tile45_AT2M_datetime.csv", row.names = FALSE)

# FUNCTION TO SUMMARIZE BY SUMMARY METRICS
summ_tile <- function(tile){
tile_summ <- tile %>%
  group_by(date) %>%
  summarize(mean_AT2m = mean(AT2m),
            max_AT2m = max(AT2m),
            min_AT2m = min(AT2m),
            sd_AT2m = sd(AT2m),
            med_AT2m = median(AT2m))
  return(tile_summ)
}

# READ THE DATA
tile32 <- read.csv("FILEPATH/tile_dfs_as_datetime/tile32_datetime.csv")
tile34 <- read.csv("FILEPATH/tile_dfs_as_datetime/tile34_datetime.csv")
tile35 <- read.csv("FILEPATH/tile_dfs_as_datetime/tile35_datetime.csv")
tile43 <- read.csv("FILEPATH/tile_dfs_as_datetime/tile43_datetime.csv")
tile44 <- read.csv("FILEPATH/tile_dfs_as_datetime/tile44_datetime.csv")
tile45 <- read.csv("FILEPATH/tile_dfs_as_datetime/tile45_datetime.csv")


tile32 <- summ_tile(tile32)
write.csv(tile32,"FILEPATH/tile32_AT2M_summ.csv", row.names = FALSE)
tile34 <- summ_tile(tile34)
write.csv(tile34,"FILEPATH/tile34_AT2M_summ.csv", row.names = FALSE)
tile35 <- summ_tile(tile35)
write.csv(tile35,"FILEPATH/tile35_AT2M_summ.csv", row.names = FALSE)
tile43 <- summ_tile(tile43)
write.csv(tile43,"FILEPATH/tile43_AT2M_summ.csv", row.names = FALSE)
tile44 <- summ_tile(tile44)
write.csv(tile44,"FILEPATH/tile44_AT2M_summ.csv", row.names = FALSE)
tile45 <- summ_tile(tile45)
write.csv(tile45,"FILEPATH/tile45_AT2M_summ.csv", row.names = FALSE)


