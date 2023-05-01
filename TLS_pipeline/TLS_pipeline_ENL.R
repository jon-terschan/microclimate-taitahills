#install.packages("XYZ")
require("lidR")
require(spdep)
require(rgeos)
require(ggthemes)
require(ggplot2)
require(plotrix)
require(plotfunctions)
require(raster)
require(fANCOVA)
require(rgeos)
require(dplyr)
require(data.table)

#### calculating ENL from CHM and PAD
setwd("FILEPATH:/")
plot_name = "las_4"
location_name = "iris-4"
project.folder = "FILEPATH:/ENL/"
voxel.size=1
pad.coefficient=0.5 

chm <- raster(paste("FILEPATH:/las_files_new/", plot_name,"/", plot_name, "_dem/CHM_", plot_name, ".tif", sep = ""))
pad <- read.csv(paste("FILEPATH:/PAD_dfs/PAD_DF_untrimmed/", plot_name,"_untrimmed.csv", sep = ""))
location.folder= paste("FILEPATH:/registered_data/", location_name, ".RiSCAN", sep = "")

###Get sp.loc.buffer
pattern = paste("*.DAT", sep="")
matrix.files <- list.files(path=location.folder, pattern=pattern)

xyz=numeric()
for (j in 1:length(matrix.files)){
  m=read.table(paste(location.folder, "/", matrix.files[j], sep=""))
  xyz=rbind(xyz, c( m[1,4], m[2,4], m[3,4]) ) 
}

xy<-xyz[,1:2]
sp.loc=SpatialPoints(xy)
sp.loc.buffer=gBuffer(sp.loc, width = 10)

#-------read multiple vox files
pattern = pattern="^merged_.*\\.vox$"
vox.files.folder=paste("FILEPATH:/winSCP/TLS_AMAPVox_Processing/",plot_name,"/",plot_name,"_output_voxel_files/", sep="")

vox.tiles.name <- list.files(path=vox.files.folder, pattern=pattern, recursive = T,  full.names = T)

all.x=numeric()
all.y=numeric()
all.k=numeric()
all.gdist=numeric()
all.pad=numeric()
for (v.name in vox.tiles.name){
  
  vox=read.table(v.name, skip=1, header = T)
  vox.info=read.table(v.name, skip=1, nrows=3, as.is = T, comment.char = "")
  
  x=vox[["i"]]
  y=vox[["j"]]
  
  realx.unique=seq(vox.info[1,2], vox.info[2,2], voxel.size)
  x.unique=unique(x)
  realy.unique=seq(vox.info[1,3], vox.info[2,3], voxel.size)
  y.unique=unique(y)
  
  ##replace xy values by same values used in original project
  for (j in 1:length(x.unique)){
    x[x==x.unique[j]]<-realx.unique[j]
  }
  for (j in 1:length(y.unique)){
    y[y==y.unique[j]]<-realy.unique[j]
  }
  
  ###read heights and pad
  k=vox[["k"]]
  g.dist=vox[["ground_distance"]]
  pad=vox[["attenuation"]]/pad.coefficient
  
  ###bind all coordinates and data togethter
  all.x=c(all.x, x)
  all.y=c(all.y, y)
  all.k=c(all.k, k)
  all.gdist=c(all.gdist, g.dist)
  all.pad=c(all.pad, pad)
}

unique.xy=unique(cbind(all.x,all.y))
m=cbind(all.x, all.y, all.k, all.gdist, all.pad)
m=m[complete.cases(m),]

###############################################
###CALCULATING FHD AND ENL1 AND 2
###############################################
#### Foliage height diversity (FHD)
#### FHD was calculated by applying the Shannon-Wiener diversity index on vertical PAI profiles
### Calculate canopy height mean and sd for the 10m tile
tile.ch=crop(chm, extent(sp.loc.buffer))
tile.mean.ch=cellStats(tile.ch, stat='mean')
tile.max.ch=cellStats(tile.ch, stat='max')
tile.sd.ch=cellStats(tile.ch, stat='sd')

### FHD and Effective number of layers:
FHD = numeric()
ENL1 = numeric()
ENL2 = numeric()

for (k in 1:nrow(unique.xy)){
  FHD.c = 0
  ENL1.c = 0
  ENL2.c = 0
  
  v.col = unique.xy[k,]
  m.sub = m[m[,1]==v.col[1] & m[,2]==v.col[2], ]
  
  column.pad = m.sub[,"all.pad"]
  p.total = sum(column.pad)
  
  if (p.total!=0) {
    ENL2.c <- 1/sum((column.pad/p.total)^2)
    for (j in 1:ceiling(tile.max.ch)){
      p.layer = (column.pad[j])/p.total
      if (p.layer!=0){
        FHD.c = FHD.c + (p.layer*log(p.layer))
      }
    }
  } else {
    ENL2.c = 0
  }
  FHD.c = - FHD.c
  FHD = rbind(FHD, c(v.col, FHD.c))
  ENL1.c = exp(FHD.c)
  ENL1 = rbind(ENL1, c(v.col, ENL1.c))
  ENL2 = rbind(ENL2, c(v.col, ENL2.c))
}

###############################################
###EXPORTING RESULTS AS RASTERS AND RASTER STATS
###############################################
#Write FHD to raster
assign("r.ch.max.FHD", crop(rasterFromXYZ(FHD, res=c(1,1), crs="", digits=5),sp.loc.buffer))
assign(paste0(plot_name, "_r.ch.max.FHD"), crop(rasterFromXYZ(FHD, res=c(1,1), crs="", digits=5),sp.loc.buffer))
writeRaster(crop(rasterFromXYZ(FHD, res = c(1,1), crs = "+proj=longlat +datum=WGS84", digits=5), sp.loc.buffer), filename =file.path(project.folder, paste0(plot_name, "_ch.max.FHD.tiff")), format="GTiff", overwrite=TRUE)
#Calculate raster stats
FHD.mean=cellStats(r.ch.max.FHD, stat='mean')
FHD.max=cellStats(r.ch.max.FHD, stat='max')
FHD.sd=cellStats(r.ch.max.FHD, stat='sd')

#Wrire ENL1 to raster
assign("r.ENL1", crop(rasterFromXYZ(ENL1, res=c(1,1), crs="", digits=5),sp.loc.buffer))
assign(paste0(plot_name, "_r.ENL1"), crop(rasterFromXYZ(ENL1, res=c(1,1), crs="", digits=5),sp.loc.buffer))
writeRaster(crop(rasterFromXYZ(ENL1, res = c(1,1), crs = "+proj=longlat +datum=WGS84", digits=5), sp.loc.buffer), filename =file.path(project.folder, paste0(plot_name, "_ENL1.tiff")), format="GTiff", overwrite=TRUE)
#Raster statistics
ENL1.mean=cellStats(r.ENL1, stat='mean')
ENL1.max=cellStats(r.ENL1, stat='max')
ENL1.sd=cellStats(r.ENL1, stat='sd')

#Write ENL2 to rasters
assign("r.ENL2", crop(rasterFromXYZ(ENL2, res=c(1,1), crs="", digits=5),sp.loc.buffer))
assign(paste0(plot_name, "_r.ENL2"), crop(rasterFromXYZ(ENL2, res=c(1,1), crs="", digits=5),sp.loc.buffer))
writeRaster(crop(rasterFromXYZ(ENL2, res = c(1,1), crs = "+proj=longlat +datum=WGS84", digits=5), sp.loc.buffer), filename =file.path(project.folder, paste0(plot_name, "_ENL2.tiff")), format="GTiff", overwrite=TRUE)
#RAster statistics
ENL2.mean=cellStats(r.ENL2, stat='mean')
ENL2.max=cellStats(r.ENL2, stat='max')
ENL2.sd=cellStats(r.ENL2, stat='sd')

# create dataframe 
las_1_ENL = as.data.frame(FHD.mean)
las_1_ENL["FHD.max"] <- FHD.max
las_1_ENL["FHD.sd"] <- FHD.sd
las_1_ENL["ENL1.mean"] <- ENL1.mean
las_1_ENL["ENL1.max"] <- ENL1.max
las_1_ENL["ENL1.sd"] <- ENL1.sd
las_1_ENL["ENL2.mean"] <- ENL2.mean
las_1_ENL["ENL2.max"] <- ENL2.max
las_1_ENL["ENL2.sd"] <- ENL2.sd
las_1_ENL["las_name"] <- plot_name

# export dataframe
write.csv(las_1_ENL, paste("FILEPATH:/ENL/summary_metrics/", plot_name, "_ENL.csv", sep = ""))

##############################################
###COMBINING SINGLE PLOTS TO UNIFIED ENL TABLE
##############################################
# adapt wd
setwd("FILEPATH:/ENL/summary_metrics")
# list all files
files <- list.files(pattern = "_ENL.csv")
# combine files
combined_files <- bind_rows(lapply(files, fread))
# delete useless column that comes with combining 
combined_files <- combined_files[,-1]
# write as csv
write.csv(combined_files, paste("FILEPATH:/ENL/summary_metrics/summary_ENL.csv", sep = ""))

#############################################
###ADDING ENL METRICS TO THE OLD METRIC TABLE
#############################################
# read combined files
combined_files <- read.csv("FILEPATH:/ENL/summary_metrics/summary_ENL.csv")
# delete useless column that comes with csv conversion
combined_files <- combined_files[,-1]
# load other metrics
stats_plus_micro <- read.csv("FILEPATH:/figures/temp_vs_metric/stats_plus_micro.csv")
# rename files so the naming convention is coherent
combined_files <- combined_files %>% 
  rename(FHD2_mean = FHD.mean,
         FHD2_max = FHD.max,
         FHD2_sd = FHD.sd,
         ENL1_mean = ENL1.mean,
         ENL1_max = ENL1.max,
         ENL1_sd = ENL1.sd,
         ENL2_mean = ENL2.mean,
         ENL2_max = ENL2.max,
         ENL2_sd = ENL2.sd)
# merge
stats_plus_micro2 <- merge(stats_plus_micro, combined_files, by = "las_name", all = T)
# export merger
write.csv(stats_plus_micro2, paste("FILEPATH:/figures/temp_vs_metric/stats_plus_micro_v2.csv", sep = ""))
