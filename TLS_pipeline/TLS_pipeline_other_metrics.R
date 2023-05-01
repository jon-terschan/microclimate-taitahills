# Calculate metrics based on AMAPVox 1.6.1
#install.packages("XYZ")
require(openxlsx)
require(geojsonio)
require(plotfunctions)
require(raster)
require(fANCOVA)
require(rgeos)
require(rgeos)
require(sp)
require(dplyr)
require(tidyverse)
require(plotrix)
require(rgdal)
require(ggplot2)
require(ggfortify)

rm(list=ls())
par(mfrow=c(1,1))
par(mar = c(1, 1, 1, 1))
################################################################################
##### ----------------------- Editable Parameters ---------------------  #######  
################################################################################

### Interval to calculate Plant Area Index                                
inicial   = 0                                                            
final     = 45                                                          
interval  = 5

# Defining the Relativity Height RH
rh = c(0.25, 0.5, 0.75, 0.95, 0.98)

# Define Main directory where all the files are in.                  
main_dir = "FILEPATH/"                          


# Registered Scans
project.folder.2 = file.path(main_dir, 'registered_data')
# LAS Folder
project.folder.3 = file.path(main_dir, 'las_data_new')
# AMAPVox Folder
project.folder.5 = file.path(main_dir, 'winSCP/TLS_AMAPVox_Processing')
# Metrics Folder
project.folder.6 = file.path(main_dir, 'Post_Processing')

### ------General script parameters--------
voxel.size = 1
pad.coefficient = 0.5 #https://doi.org/10.1016/j.rse.2018.06.024

######## -------- END editable parameters --------------- ########
################################################################################

################################################################################              
# List with folder in the 2-Registered
list = sort(list.files(project.folder.2, full.names = T))
list
#i=3

for (i in 1:length(list)){
  
  location.folder = list[i]
  plot_name = sort(list.files(project.folder.3))[i]
  #plot_name = substr(plot_name, 1,nchar(plot_name)-4)
  
  
  ### set folders
  vox.files.folder = file.path(project.folder.5, plot_name,  paste0(plot_name ,"_output_voxel_files/"))
  
  ### Read CHM
  chm = raster(file.path(project.folder.3, paste0(plot_name) , paste0(plot_name,"_dem/"), paste0("CHM_" ,plot_name,".tif")))
  
  ### Get scan locations and create a buffer around them
  folder_list = list.files(project.folder.2)
  matrix.files = list.files(file.path(project.folder.2, folder_list[i]), pattern = paste("*.DAT", sep=""))
  
  xyz = numeric()
  for (j in 1:length(matrix.files)){
    m = read.table(file.path(project.folder.2, folder_list[i], matrix.files[j]))
    xyz = rbind(xyz, c( m[1,4], m[2,4], m[3,4]) )
  }
  
  xy = xyz[,1:2]
  sp.loc = SpatialPoints(xy)
  sp.loc.buffer = gBuffer(sp.loc, width = 2.5)
  
  ### Read multiple vox files
  vox.tiles.name = list.files(vox.files.folder, pattern = "^merged_.*\\.vox$", recursive = T,  full.names = T)
  
  all.x = numeric()
  all.y = numeric()
  all.k = numeric()
  all.gdist = numeric()
  all.pad = numeric()
  
  for (v.name in 1:length(vox.tiles.name)){
    print(v.name)
    flush.console()
    
    vox = read.table(vox.tiles.name[v.name], skip = 1, header = T)
    vox.info = read.table(vox.tiles.name[v.name], skip = 1, nrows=3, as.is = T, comment.char = "")
    
    x = vox[["i"]]
    y = vox[["j"]]
    
    realx.unique = seq(vox.info[1,2], vox.info[2,2], voxel.size)
    x.unique = unique(x)
    realy.unique = seq(vox.info[1,3], vox.info[2,3], voxel.size)
    y.unique = unique(y)
    
    ### Replace xy values by same values used in original project
    for (j in 1:length(x.unique)){
      x[x==x.unique[j]]<-realx.unique[j]
    }
    for (j in 1:length(y.unique)){
      y[y==y.unique[j]]<-realy.unique[j]
    }
    ### Read heights and PAD
    k = vox[["k"]]
    g.dist = vox[["ground_distance"]]
    pad = vox[["attenuation"]] / pad.coefficient
    
    ### Bind all coordinates and data together
    all.x = c(all.x, x)
    all.y = c(all.y, y)
    all.k = c(all.k, k)
    all.gdist = c(all.gdist, g.dist)
    all.pad = c(all.pad, pad)
  }
  
  unique.xy = unique(cbind(all.x,all.y))
  unique.xy.sp = SpatialPoints(unique.xy)
  
  m = cbind(all.x, all.y, all.k, all.gdist, all.pad)
  m = m[complete.cases(m),] #remove NA = below ground voxels
  
  m[m[,"all.gdist"] <= 0.5,] <- NA
  m = m[complete.cases(m),] #remove NA = below 0.5
  #####################################
  ##location of voxels
  plot(chm, main =plot_name)
  # plot(unique.xy.sp, add = T)
  plot(sp.loc.buffer, add = T)
  ##################################
  
  
  #################################
  ### Calculate PAI for each column
  pai = numeric()
  for (k in 1:nrow(unique.xy)){
    v.col = unique.xy[k,]
    m.sub = m[m[,1] == v.col[1] & m[,2] == v.col[2], ]
    PAI.col = sum(m.sub[,"all.pad"], na.rm = T)
    pai = rbind(pai, c(v.col, PAI.col))
  }
  
  assign(paste0("r.pai"), crop(rasterFromXYZ(pai, res = c(1,1), crs="", digits = 5), sp.loc.buffer))
  if (dir.exists(file.path(project.folder.6,plot_name)) == F) {
    dir.create(file.path(project.folder.6,plot_name))
  }
  writeRaster(crop(rasterFromXYZ(pai, res = c(1,1), crs="+proj=longlat +datum=WGS84", digits = 5),sp.loc.buffer), filename = file.path(project.folder.6,plot_name, paste0(plot_name,"_pai.tiff")), format="GTiff", overwrite=TRUE)
  
  
  
  ##### Plant area index (PAI) per 5 m height layers
  ###### (PAI0-10, PAI10-20, PAI20-30, PAI30-40)
  ###### PAI0-10 of 2 means that there are 2 m2 of leaves and branches per m2 ground within 10 m from the ground
  sq = seq(inicial, final - interval, by = interval)
  
  temp = numeric()
  for (k in sq){
    for (l in 1:nrow(unique.xy)){
      v.col = unique.xy[l,]
      m.sub = m[m[,1]==v.col[1] & m[,2]==v.col[2], ]
      PAI.col = sum(m.sub[,"all.pad"][m.sub[,"all.gdist"] > k & m.sub[,"all.gdist"] < k + 5], na.rm=T)
      temp = rbind(temp, c(v.col, PAI.col))
    }
    
    assign(paste0("pai_", k, "_", k + interval), temp)
    assign(paste0("r.pai_", k, "_", k + interval), crop(rasterFromXYZ(temp, res = c(1,1), crs="", digits = 5),sp.loc.buffer))
    writeRaster(crop(rasterFromXYZ(temp, res = c(1,1), crs="+proj=longlat +datum=WGS84", digits = 5),sp.loc.buffer), filename = file.path(project.folder.6,plot_name, paste0(plot_name,"_pai_", k, "_", k + interval, ".tiff")), format="GTiff", overwrite=TRUE)
    
    
    temp = NULL
    temp = numeric()
  }
  ### Calculate canopy height mean and sd for the 10m tile
  # tile.ch=crop(chm, extent(xy.sp))
  # tile.mean.ch=cellStats(tile.ch, stat='mean')
  # tile.sd.ch=cellStats(tile.ch, stat='sd')
  # boxplot(tile.ch)
  
  #### Relative heights (RH) as percentiles of the vertical distribution of canopy
  #### points at 25, 50, 75 and 95%
  #### An RH25 value of 6 m means that 25% of energy is located below 6 m from the ground
  for (p in 1:length(rh)){
    temp2 = numeric()
    for (k in 1:nrow(unique.xy)){
      v.col = unique.xy[k,]
      m.sub = m[m[,1]==v.col[1] & m[,2]==v.col[2], ]
      column.pad = m.sub[,"all.pad"]  
      p.total = sum(column.pad)
      max.height = max(m.sub[,"all.gdist"], na.rm=T)
      if (p.total!=0) { ##only calculates the RH if the total PAD different than zero
        perc = cumsum(column.pad)/max(cumsum(column.pad))
        height = m.sub[,"all.gdist"]
        #plot(perc, height)
        #########################################
        x0 = tail(perc[perc < rh[p]], n=1)
        x1 = head(perc[perc > rh[p]], n=1)
        y0 = tail(height[perc < rh[p]], n=1)
        y1 = head(height[perc > rh[p]], n=1)
        
        if (length(x0)==0){
          x0 = 0
          y0 = 0
        }
        x = c(x0, x1)
        y = c(y0, y1)
        f <- approxfun(c(x0, x1), c(y0, y1))
        
        temp2 = rbind(temp2, c(v.col, f(rh[p]) ))
      }else{
        temp2 = rbind(temp2, c(v.col, 0))
      }
    }
    
    assign(paste0("RH", rh[p]*100), temp2)
    assign(paste0("r.RH", rh[p]*100), crop(rasterFromXYZ(temp2, res = c(1,1), crs="", digits = 5),sp.loc.buffer))
    writeRaster(crop(rasterFromXYZ(temp2, res = c(1,1), crs="+proj=longlat +datum=WGS84", digits = 5),sp.loc.buffer), filename =file.path(project.folder.6,plot_name,  paste0(plot_name,"_RH", rh[p]*100,".tiff")), format="GTiff", overwrite=TRUE)
    
  }
  
  ### Canopy ratio (CR) as the percentage of canopy depth to canopy height:
  CR = RH98
  CR[,3] <- (RH98[,3] - RH25[,3]) / RH98[,3]
  
  assign("r.CR", crop(rasterFromXYZ(CR, res=c(1,1), crs="", digits=5),sp.loc.buffer))
  assign(paste0(plot_name, "_r.CR"), crop(rasterFromXYZ(CR, res=c(1,1), crs="", digits=5),sp.loc.buffer))
  writeRaster(crop(rasterFromXYZ(CR, res=c(1,1), crs="+proj=longlat +datum=WGS84", digits=5), sp.loc.buffer), filename = file.path(project.folder.6,plot_name,  paste0(plot_name, "_CR.tiff")), format="GTiff", overwrite=TRUE)
  
  
  #### Foliage height diversity (FHD)
  #### FHD was calculated by applying the Shannon-Wiener diversity index on vertical PAI profiles
  
  ### Calculate canopy height mean and sd for the 10m tile
  #tile.ch=crop(chm, extent(sp.loc.buffer))
  #tile.mean.ch=cellStats(tile.ch, stat='mean')
  #tile.max.ch=cellStats(tile.ch, stat='max')
  #tile.sd.ch=cellStats(tile.ch, stat='sd')
  #boxplot(tile.ch)
  
  FHD = numeric()
  for (k in 1:nrow(unique.xy)){
    FHD.c = 0
    
    v.col = unique.xy[k,]
    m.sub = m[m[,1]==v.col[1] & m[,2]==v.col[2], ]
    
    column.pad = m.sub[,"all.pad"]  
    p.total = sum(column.pad)
    
    if (p.total!=0) {
      for (j in 1:nrow(m.sub)){
        p.layer = (column.pad[j])/p.total
        if (p.layer!=0){
          FHD.c = FHD.c + (p.layer*log(p.layer))}
      }
    }
    FHD.c = - FHD.c
    FHD = rbind(FHD, c(v.col, FHD.c))
  }
  assign("r.FHD", crop(rasterFromXYZ(FHD, res=c(1,1), crs="", digits=5),sp.loc.buffer))
  assign(paste0(plot_name, "r.FHD"), crop(rasterFromXYZ(FHD, res=c(1,1), crs="", digits=5),sp.loc.buffer))
  writeRaster(crop(rasterFromXYZ(FHD, res = c(1,1), crs = "+proj=longlat +datum=WGS84", digits=5), sp.loc.buffer), filename =file.path(project.folder.6,plot_name,  paste0(plot_name, "_FHD.tiff")), format="GTiff", overwrite=TRUE)
  
  #### Foliage Height Diversity in 5m layers (like PAI)
  #FHD = numeric()
  #sq = seq(inicial, final - interval, by = interval)
  #h=0
  #for (h in sq){
  #for (k in 1:nrow(unique.xy)){
  # FHD.c = 0
  #v.col = unique.xy[k,]
  #m.sub = m[m[,1]==v.col[1] & m[,2]==v.col[2], ]
  
  #column.pad = m.sub[,"all.pad"]
  #p.total = sum(column.pad)
  
  #if (p.total!=0) {
  # for (h in sq){
  #  p.layer = sum(m.sub[,"all.pad"][m.sub[,"all.gdist"] > h & m.sub[,"all.gdist"] < h + 5], na.rm=T)
  # p.layer = p.layer/p.total
  #if (p.layer!=0){
  # FHD.c = FHD.c + (p.layer*log(p.layer))
  #}
  #}
  #}
  #FHD.c = - FHD.c
  #FHD = rbind(FHD, c(v.col, FHD.c))
  #}
  #assign("r.5m.FHD", crop(rasterFromXYZ(FHD, res=c(1,1), crs="", digits=5),sp.loc.buffer))
  #assign(paste0(plot_name, "r.5m.FHD"), crop(rasterFromXYZ(FHD, res=c(1,1), crs="", digits=5),sp.loc.buffer))
  #writeRaster(crop(rasterFromXYZ(FHD, res = c(1,1), crs = "+proj=longlat +datum=WGS84", digits=5), sp.loc.buffer), filename =file.path(project.folder.6,plot_name,  paste0(plot_name, "_5m_FHD.tiff")), format="GTiff", overwrite=TRUE)
  
  #####################################################################
  ########################### SAVE ALL ###############################
  
  if (dir.exists(file.path(project.folder.6,plot_name,paste0(plot_name, "_metrics"))) == F) {
    dir.create(file.path(project.folder.6,plot_name,paste0(plot_name, "_metrics")))
  }
  save(m, sp.loc, sp.loc.buffer,
       FHD,
       r.FHD,
       CR, r.CR,
       RH25, RH50, RH75, RH95, RH98,
       r.RH25, r.RH50, r.RH75, r.RH95, r.RH98,
       pai_0_5, pai_5_10, pai_10_15, pai_15_20, pai_20_25, pai_25_30, pai_30_35, pai_35_40,
       r.pai_0_5, r.pai_5_10, r.pai_10_15, r.pai_15_20, r.pai_20_25, r.pai_25_30, r.pai_30_35, r.pai_35_40,
       pai, r.pai,
       file = file.path(project.folder.6, plot_name, paste0(plot_name,"_metrics"), paste0(plot_name,"_metrics_v1.RData"))
  )
}


# "PAD [m2/m3]"
# list = sort(list.files(project.folder.6, full.names = T))
# par(mfrow=c(5,4))
#
# for (i in 1:length( sort(list.files(project.folder.6, full.names = T)))){
#   plot_name = sort(list.files(project.folder.6))[i]
#  
#   load(file.path(list[i],paste0(plot_name,"_metrics"), paste0(plot_name,"_metrics_v1.RData")))
#
#   h = hist(r.pai, breaks  = seq(0,100 , by = 4), plot=F)
#   h$density = h$counts/sum(h$counts) * 100
#  
#   plot(h, freq = FALSE, axes =F,
#        #labels = paste0(round(h$density,2), "%"),
#        ylim = c(0, 75), ylab = "",
#        xlim = c(0, 40), xlab = "",
#        main = (plot_name),
#        labels = paste0(round(h$density, digits = 2), "%"))
#  
#   axis(side = 1, pos = 0, at = c(seq(0, 40, by = 4)))
#   axis(side = 2, pos = 0, at = c(seq(0,105, by = 5)))    
#   mtext(side = 1, text = "PAD", line = 2)
#   mtext(side = 2, text = "Frequency (%)", line = 1)
# }

##########################################################################
##########################################################################
####################SUMMARY METRICS#######################################
##########################################################################
##########################################################################

# set plot name
plot_name = "las_17"
# set wd
setwd("FILEPATH//")

# quick renaming function
rename_col <- function(df, colname){
  colnames(df)[3] <- colname
  return(df)
}
# rename third column to something that makes sense
FHD <- rename_col(FHD, "FHD")
CR <- rename_col(CR, "CR")
pai <- rename_col(pai, "pai")
RH25 <- rename_col(RH25, "RH25")
RH50 <- rename_col(RH50, "RH50")
RH75 <- rename_col(RH75, "RH75")
RH95 <- rename_col(RH95, "RH95")
RH98 <- rename_col(RH98, "RH98")

# combine all metrics into a single table.
df_list <- list(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98)
combined_metrics <- Reduce(function(x, y) merge(x, y, by=c("all.x", "all.y")), df_list)
combined_metrics$name <- plot_name
# export combined metrics in a better way
write.csv(combined_metrics, paste("Post_Processing/", plot_name, "/", plot_name, "_combined_metrics.csv", sep = ""))

combined_metrics$mean <- "Mean"
combined_metrics$median <- "Median"
combined_metrics$max <- "Max"
combined_metrics$min <- "Min"
combined_metrics$sd <- "Std. Deviation"
combined_metrics$se <- "Std. Error"
combined_metrics$qn5 <- "Qnt_5"
combined_metrics$qn25 <- "Qnt_25"
combined_metrics$qn75 <- "Qnt_75"
combined_metrics$qn95 <- "Qnt_95"

# theres probably a more efficient way to do this
mean_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ mean, 
                       data = combined_metrics, 
                       FUN = function(i) mean(i, na.rm = T))
med_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ median, 
          data = combined_metrics, 
          FUN = function(i) median(i, na.rm = T))
max_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ max, 
                      data = combined_metrics, 
                      FUN = function(i) max(i, na.rm = T))
min_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ min, 
                      data = combined_metrics, 
                      FUN = function(i) min(i, na.rm = T))
sd_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ sd, 
                     data = combined_metrics, 
                     FUN = function(i) sd(i, na.rm = T))
se_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ se, 
                 data = combined_metrics, 
                 FUN = function(i) std.error(i, na.rm = T))
qn5_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ qn5, 
                      data = combined_metrics, 
                      FUN = function(i) quantile(i, probs = 0.05, na.rm = T))
qn25_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ qn25, 
                       data = combined_metrics, 
                       FUN = function(i) quantile(i, probs = 0.25, na.rm = T))
qn75_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ qn75, 
                       data = combined_metrics, 
                       FUN = function(i) quantile(i, probs = 0.75, na.rm = T))
qn95_las1 <- aggregate(cbind(FHD, CR, pai, RH25, RH50, RH75, RH95, RH98) ~ qn95, 
                       data = combined_metrics, 
                       FUN = function(i) quantile(i, probs = 0.95, na.rm = T))

df_list2 <- list(mean_las1, med_las1, max_las1, min_las1, sd_las1, se_las1,
                 qn5_las1, qn25_las1, qn75_las1, qn95_las1)

# another renaming function to unify the metric row
rename_col2 <- function(df) {
  colnames(df)[1] <- "Metric"  
  return(df)
}
# apply renaming to list of dfs
dfs <- lapply(df_list2, rename_col2)

# combine renamed list of dfs into one df
sum_metrics <- bind_rows(dfs)
# add column with plot name 
sum_metrics$plot_name <- plot_name
# export that biach
write.csv(sum_metrics, paste("Post_Processing/summary_metrics_by_row/", plot_name, "_summ_by_rows.csv", sep = ""))


# # essentially the same as aggregate but all in one and structured on columns
# deprecated but might be needed for machine readability, adapt if necessary

# #sum_metrics_las_1 <- combined_metrics %>%
#   #summarize(avg_FHD = mean(FHD, na.rm = TRUE),
#             avg_CR = mean(CR, na.rm = TRUE),
#             avg_pai = mean(pai, na.rm = TRUE),
#             avg_RH25 = mean(RH25, na.rm = TRUE),
#             avg_RH50 = mean(RH50, na.rm = TRUE),
#             avg_RH75 = mean(RH75, na.rm = TRUE),
#             avg_RH95 = mean(RH95, na.rm = TRUE),
#             avg_RH98 = mean(RH98, na.rm = TRUE),
#             med_FHD = median(FHD, na.rm = TRUE),
#             med_CR = median(CR, na.rm = TRUE),
#             med_pai = median(pai, na.rm = TRUE),
#             med_RH25 = median(RH25, na.rm = TRUE),
#             med_RH50 = median(RH50, na.rm = TRUE),
#             med_RH75 = median(RH75, na.rm = TRUE),
#             med_RH95 = median(RH95, na.rm = TRUE),
#             med_RH98 = median(RH98, na.rm = TRUE),
#             min_CR = min(CR, na.rm = TRUE),
#             min_pai = min(pai, na.rm = TRUE),
#             min_RH25 = min(RH25, na.rm = TRUE),
#             min_RH50 = min(RH50, na.rm = TRUE),
#             min_RH75 = min(RH75, na.rm = TRUE),
#             min_RH95 = min(RH95, na.rm = TRUE),
#             min_RH98 = min(RH98, na.rm = TRUE),
#             max_CR = max(CR, na.rm = TRUE),
#             max_pai = max(pai, na.rm = TRUE),
#             max_RH25 = max(RH25, na.rm = TRUE),
#             max_RH50 = max(RH50, na.rm = TRUE),
#             max_RH75 = max(RH75, na.rm = TRUE),
#             max_RH95 = max(RH95, na.rm = TRUE),
#             max_RH98 = max(RH98, na.rm = TRUE),
#             sd_CR = sd(CR, na.rm = TRUE),
#             sd_pai = sd(pai, na.rm = TRUE),
#             sd_RH25 = sd(RH25, na.rm = TRUE),
#             sd_RH50 = sd(RH50, na.rm = TRUE),
#             sd_RH75 = sd(RH75, na.rm = TRUE),
#             sd_RH95 = sd(RH95, na.rm = TRUE),
#             sd_RH98 = sd(RH98, na.rm = TRUE),
#             se_CR = std.error(CR, na.rm = TRUE),
#             se_pai = std.error(pai, na.rm = TRUE),
#             se_RH25 = std.error(RH25, na.rm = TRUE),
#             se_RH50 = std.error(RH50, na.rm = TRUE),
#             se_RH75 = std.error(RH75, na.rm = TRUE),
#             se_RH95 = std.error(RH95, na.rm = TRUE),
#             se_RH98 = std.error(RH98, na.rm = TRUE)
#   )
# # add column with plot name
# sum_metrics_las_1$plot_name <- plot_name
# # export that biach
# write.csv(sum_metrics_las_1, paste("Post_Processing/", plot_name, "/", plot_name, "_summary_by_rows/", plot_name, "_summ_by_col.csv", sep = ""))
# 

# create a list of all summary metric files
my_files <- list.files("FILEPATH/Post_Processing/summary_metrics_by_row", pattern = "\\.csv$")
# adapt wd for lapply
setwd("FILEPATH/Post_Processing/summary_metrics_by_row")
# read all files
sum_metric_list <- lapply(my_files, read.csv)
# combine all plots to a single file
all_sum_metrics <- bind_rows(sum_metric_list)
# change back wd 
setwd("FILEPATH//")
# import veg type descriptions
veg_type <- read.csv("FILEPATH/Post_Processing/description_kenya_plots.csv", sep = ";")
all_sum_metrics <- read.csv("FILEPATH/Post_Processing/summary_metrics_by_row/all_summ_by_rows.csv")
#drop old columns deprecated
#sum_metrics <- sum_metrics[-c(1, 2, 13, 14, 15)]
# join tomst sensor df with sum metrics df by plotname
all_sum_metrics <- left_join(all_sum_metrics, 
                             select(veg_type, plot_name, veg_type, class, obs), 
                             by = "plot_name")
# rename column names to streamline the df
  #sum_metrics <- plyr::rename(sum_metrics, c("NOTES" = "veg_type",
    #"Metric" = "metric",
    #"ELEVATION" = "elevation",
    #"LOCATION" = "location"))
# export all summary metrics and added veg type descriptors
getwd()
write.csv(all_sum_metrics, paste("FILEPATH/Post_Processing/summary_metrics_by_row/all_summ_by_rows.csv", sep = ""))


##########################################################################
##########################################################################
####################  PCA ANALYSIS  ######################################
##########################################################################
##########################################################################
# importing the metrics
setwd("F:/")
all_sum_metrics <- read.csv("F:/Post_Processing/summary_metrics_by_row/all_summ_by_rows.csv", sep = ",")

# pca for metric and corresponding rows
pca_med <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Median",3:10], scale = T)
pca_mean <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Mean",3:10], scale = T)
pca_max <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Max",3:10], scale = T)
pca_min <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Min",3:10], scale = T)
pca_sd <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Std. Deviation",3:10], scale = T)
pca_se <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Std. Error",3:10], scale = T)
pca_qnt75 <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Qnt_75",3:10], scale = T)
pca_qnt5 <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Qnt_5",3:10], scale = T)
pca_qnt25 <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Qnt_25",3:10], scale = T)
pca_qnt95 <- prcomp(all_sum_metrics[all_sum_metrics$metric == "Qnt_95",3:10], scale = T)

# scree plots total variance, custom function i found 
# https://www.statology.org/scree-plot-r/
# getname <- function(x) deparse(substitute(x))
screevariance <- function(data){
  var_explained = data$sdev^2 / sum(data$sdev^2)
  return(qplot(c(1:8), var_explained) + 
    geom_line(size = 0.7) + 
    geom_point(size = 2) +
    xlab("Principal Component") + 
    ylab("Total Variance Explained") +
    ggtitle(paste0("Scree Plot", getname(data), sep = "")) +
    ylim(0, 1))
}




# screeplots with PCA1 >0.75 total variance
screevariance(pca_med)
screevariance(pca_mean)
screevariance(pca_qnt25)
screevariance(pca_qnt75)
screevariance(pca_qnt5)
screevariance(pca_se)
# screeplots with PCA1 <0.75 total variance
screevariance(pca_max)
screevariance(pca_min)
screevariance(pca_qnt95)
screevariance(pca_sd)
# eigenvalues instead of variance but essentially the same takeaway
# kaiser criterion filters eigenvalue according to > or < 1 so we do that too
# screeplots where PCA2+ eigenvalue <1
screeplot(pca_med)
screeplot(pca_mean)
screeplot(pca_qnt25)
screeplot(pca_qnt75)
screeplot(pca_qnt5)
screeplot(pca_se)
# screeplots with PCA2 eigenvalue >1
screeplot(pca_max)
screeplot(pca_min)
screeplot(pca_qnt95)
screeplot(pca_sd)

# eigenvalue table
eigenvalue_list <- list("max" = pca_max[2],
                        "mean" = pca_mean[2],
                        "median" = pca_med[2],
                        "min" = pca_min[2],
                        "qnt25" = pca_qnt25[2],
                        "qnt5" = pca_qnt5[2],
                        "qnt75" = pca_qnt75[2],
                        "qnt95" = pca_qnt95[2],
                        "sd" = pca_sd[2],
                        "se" = pca_se[2]
          )




# export eigenvalues for further analysis
openxlsx::write.xlsx(eigenvalue_list, 
                     "FILEPATH/Post_Processing/PCA/pca_eigenvalues.xlsx", 
                     col.names = T, 
                     row.names = T)

# remove certain metrics
all_sum_metrics_less = all_sum_metrics[,-(7:9)]
# if not wanted
all_sum_metrics_less <- all_sum_metrics

# plot linings backup 
autoplot(prcomp(all_sum_metrics_less[all_sum_metrics_less$metric == "Mean",3:10], scale = T), data = all_sum_metrics_less[all_sum_metrics_less$metric == "Mean",], 
        shape = F, colour = "veg_type", loadings = TRUE,loadings.label = T) +
  geom_text(aes(label=as.character(veg_type)),hjust=0,vjust=0)

# experimental
autoplot(prcomp(all_sum_metrics_less[all_sum_metrics_less$metric == "Median",3:10], scale = T), data = all_sum_metrics_less[all_sum_metrics_less$metric == "Median",], 
         shape = T, colour = "veg_type", loadings = TRUE, loadings.label = T) +
  geom_text(aes(label=as.character(veg_type)),hjust=0,vjust=0)+
  geom_point(size=2)


