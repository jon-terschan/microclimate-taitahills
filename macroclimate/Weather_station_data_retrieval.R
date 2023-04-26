# INSTALL AND ACTIVATE DEPENDENCIES 
# install.packages("XYZ")
library(dplyr)
library(Thermimage) # FOR CONVERTING BINARY RAW DATA INTO TEMPERATURE ESTIMATES
library(ggplot2)

# LOAD RESEARCH STATION DATA 
taita_research_station <- read.csv("FILEPATH/Taita_Hills_Research_Station.dat")
kishenyi_station <- read.csv("FILEPATH/CR1000_2_Kishenyi.dat")
maktau_station <- read.csv("FILEPATH/CR3000_Maktau.dat")

# MAKTAU STATION: TRIM TO STUDY PERIOD AND REMOVE EVERY 30 MIN OBSERVATION 
##########################################################################
# TRIM TO STUDY PERIOD (1:4 IF RELATIVE HUMIDTY IS DESIRED)
maktau_station_subset <- maktau_station[36261:48980, 1:3]
# REMOVE UNNECESSARY COLUMN
maktau_station_subset <- subset(maktau_station_subset, select = -2)
# TRANSFORM DATA TYPES TO DATETIME (POSIXCT) AND NUMERIC
maktau_station_subset[,1] <- as.POSIXct(maktau_station_subset[,1],tz="UTC")
maktau_station_subset[,2] <- as.numeric(maktau_station_subset[,2])
#maktau_station_subset[,3] <- as.numeric(maktau_station_subset[,3])
# DELETE EVERY SECOND ROW
maktau_station_trim <- 
  maktau_station_subset %>% filter(row_number() %% 2 != 1) ## Delete odd-rows
# FILL COLUMN WITH MEAN FOR EVERY SECOND COLUMN
maktau_station_trim[,2] <- meanEveryN(maktau_station_subset[,2], n =2)
# SAVE
write.csv(maktau_station_trim,"FILEPATH/Maktau_trimmed.csv", row.names = FALSE)

# KISHENYI STATION: TRIM TO STUDY PERIOD AND REMOVE EVERY 30 MIN OBSERVATION
############################################################################
kishenyi_station_subset <- kishenyi_station[59841:72560,1:3]
kishenyi_station_subset <- subset(kishenyi_station_subset, select = -2)
kishenyi_station_subset[,1] <- as.POSIXct(kishenyi_station_subset[,1],tz="UTC")
kishenyi_station_subset[,2] <- as.numeric(kishenyi_station_subset[,2])
#kishenyi_station_subset[,3] <- as.numeric(kishenyi_station_subset[,3])
kishenyi_station_trim <- 
  kishenyi_station_subset %>% filter(row_number() %% 2 != 1) ## Delete odd-rows
kishenyi_station_trim[,2] <- meanEveryN(kishenyi_station_subset[,2], n =2)

write.csv(kishenyi_station_trim,"FILEPATH/Kishenyi_trimmed.csv", row.names = FALSE)

# TAITA HILLS RESEARCH STATION: TRIM TO STUDY PERIOD, CHANGE DATETIME TO POSICX
###############################################################################
taita_research_station_subset <- taita_research_station[52790:59149, 1:2]
taita_research_station_subset[,1] <- as.POSIXct(taita_research_station_subset[,1],tz="UTC")
# CHECK DATA TYPE 
#str(taita_research_station_subset)
write.csv(taita_research_station_subset,"FILEPATH/TaitaHillsRS_trimmed.csv", row.names = FALSE)

# CHECK RELATIVE HUMIDITY (DEPRECATED)
# par(mfrow=c(3,1))
# plot(taita_research_station_subset$X, taita_research_station_subset$RH, type = "l")
# plot(kishenyi_station_trim$TIMESTAMP, kishenyi_station_trim$RH, type = "l")
# plot(maktau_station_trim$TIMESTAMP, maktau_station_trim$RH, type = "l")
# 
# mean(as.numeric(maktau_station_trim$RH))
# mean(as.numeric(taita_research_station_subset$RH))
# mean(as.numeric(kishenyi_station_trim$RH))
# summary(kishenyi_station_trim)
# summary(maktau_station_trim)
# summary(taita_research_station_subset)
# sd(kishenyi_station_trim$RH)
# sd(maktau_station_trim$RH)
# sd(taita_research_station_subset$RH)
