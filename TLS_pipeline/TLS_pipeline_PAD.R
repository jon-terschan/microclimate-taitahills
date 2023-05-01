#open puhti job
#sinteractive --account project_2001208 --mem 12000 --tmp 100
#module load r-env-singularity

#R


# call required libraries
require("lidR")
require(spdep)
require(rgeos)

# remove environment
rm(list=ls())

######Settings
space.size=10
max.tree.height=50 #maximum tree height in meters
# CHANGE
project.folder="/scratch/project_2001208/FIRSTNAME/Registered/iris-17.RiSCAN"
# CHANGE
plot_name="las_17"
##############load info from scans Matrices
pattern = paste("*.DAT", sep="")
matrix.files <- list.files(path=project.folder, pattern=pattern)

xyz=numeric()
for (j in 1:length(matrix.files)){
  m=read.table(paste(project.folder, "/", matrix.files[j], sep=""))
  xyz=rbind(xyz, c( m[1,4], m[2,4], m[3,4]) ) 
}

##load scans location
#loca-tion.file="C:/LocalData/eemaeda/RIEGL_Scans/Colosso_10h_April_tiles_ground/COLOSSO-10ha.CSV"
#loc=read.table(location.file, sep=",")
#xy <- loc[,2:3]
xy<-xyz[,1:2]
sp.loc=SpatialPoints(xy)
sp.loc.buffer=gBuffer(sp.loc, width = 10)

x.max=max(xy[,1])+space.size
y.max=max(xy[,2])+space.size
x.min=min(xy[,1])-space.size
y.min=min(xy[,2])-space.size

z.max=max(xyz[,3])+max.tree.height 
z.min=min(xyz[,3])-2 #considering 2 m scan height

r <- raster(ext = extent(x.min, x.max, y.min, y.max), res=space.size)
values(r) <- 1:ncell(r)


#r.crop=mask(r, sp.loc.buffer) #mask function is keeping partial pixels out, so found this solution intead
r.crop <- rasterize(sp.loc.buffer, r, getCover=TRUE)
r.crop[r.crop==0] <- NA

p <- rasterToPolygons(r.crop) 

plot(r)
plot(p, add=T, col="red")
plot(sp.loc, add=T)
plot(sp.loc.buffer, add=T)

plot(p[1,], add=T, col="blue")

voxel_space_list=numeric()

for (i in 1:length(p)){
  i_space=c(extent(p[i,])@xmin, extent(p[i,])@xmax, extent(p[i,])@ymin, extent(p[i,])@ymax, z.min, z.max)
  voxel_space_list=rbind(voxel_space_list, i_space)
  
}

rownames(voxel_space_list)<- as.character(seq(1:length(p)))
colnames(voxel_space_list)<- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")

print(voxel_space_list)

#save files in tables
setwd("/scratch/project_2001208/FIRSTNAME/las_data/TLS_AMAPVox_Processing/las_17/las_17_Voxel_space_tables")

save(voxel_space_list,
     file = paste("voxel_space_list_",plot_name,".RData", sep="") )

##########################################
##########################################
##########################################
#In R create multiple XML config files 
require(XML)

rm(list=ls())

######Settings
plot_name="las_17"
project.folder="/scratch/project_2001208/FIRSTNAME/Registered/iris-17.RiSCAN/" ##Leave last "/"

##################################

new.project.file=paste(project.folder, "project.rsp", sep="")
new.scan.folder=paste(project.folder, "SCANS/", sep="")
new.dtm.folder=paste("/scratch/project_2001208/FIRSTNAME/las_data/TLS_AMAPVox_Processing/DTMs","/DTM_",plot_name,".asc", sep="")
puhti.amapvox.out.folders=paste("/scratch/project_2001208/FIRSTNAME/las_data/TLS_AMAPVox_Processing")
voxel.space.tables.folder=paste("/scratch/project_2001208/FIRSTNAME/las_data/TLS_AMAPVox_Processing/",plot_name,"/", plot_name,"_Voxel_space_tables/", sep="")

config.file.folder=paste("/scratch/project_2001208/FIRSTNAME/las_data/TLS_AMAPVox_Processing/",plot_name,"/", plot_name,"_Config_files/", sep="")

  #############Change input and output folders

setwd(voxel.space.tables.folder)
load(paste("voxel_space_list_",plot_name,".RData", sep=""))

n.files=nrow(voxel_space_list)

setwd(config.file.folder)

for (i in 1:n.files){
  
  all.space.values=voxel_space_list[i,]
  
  new.xmin=format(round(all.space.values["xmin"], 3), nsmall = 3)
  new.xmax=format(round(all.space.values["xmax"], 3), nsmall = 3)
  new.ymin=format(round(all.space.values["ymin"], 3), nsmall = 3)
  new.ymax=format(round(all.space.values["ymax"], 3), nsmall = 3)
  new.zmin=format(round(all.space.values["zmin"], 0), nsmall = 0)
  new.zmax=format(round(all.space.values["zmax"], 0), nsmall = 0)
  
  new.splitX=as.character(as.numeric(new.xmax)-as.numeric(new.xmin))
  new.splitY=as.character(as.numeric(new.ymax)-as.numeric(new.ymin))
  new.splitZ=as.character(as.numeric(new.zmax)-as.numeric(new.zmin))
  
  #<voxelspace xmin="4.0" ymin="-23.0" zmin="1779.0" xmax="26.0" ymax="-1.0" zmax="1829.0" splitX="22" splitY="22" splitZ="50" resolution="1.0" subvoxel="2"/>
  xml <- xmlTreeParse(paste(plot_name,"_config.xml", sep=""))
  #xml$doc$children$configuration[1]$process[3]$voxelspace
  
  root = xmlRoot(xml)
  
  xmlAttrs(root[["process"]][["voxelspace"]])[["xmin"]] <- new.xmin 
  xmlAttrs(root[["process"]][["voxelspace"]])[["xmax"]] <- new.xmax
  xmlAttrs(root[["process"]][["voxelspace"]])[["ymin"]] <- new.ymin
  xmlAttrs(root[["process"]][["voxelspace"]])[["ymax"]] <- new.ymax  
  xmlAttrs(root[["process"]][["voxelspace"]])[["zmin"]] <- new.zmin
  xmlAttrs(root[["process"]][["voxelspace"]])[["zmax"]] <- new.zmax
  
  xmlAttrs(root[["process"]][["voxelspace"]])[["splitX"]] <- new.splitX
  xmlAttrs(root[["process"]][["voxelspace"]])[["splitY"]] <- new.splitY
  xmlAttrs(root[["process"]][["voxelspace"]])[["splitZ"]] <- new.splitZ
  
  #############Change input and output folders
  new.output.folder=paste(puhti.amapvox.out.folders,"/",plot_name, "/", plot_name,"_output_voxel_files/out_",plot_name,"_tile-",i, sep="")
  
  
  xmlAttrs( root[["process"]][["input"]][["main_file"]])[["src"]] <- new.project.file
  
  xmlAttrs( root[["process"]][["output"]][["voxel_file"]])[["src"]] <- new.output.folder
  xmlAttrs( root[["process"]][["output"]][["merging"]])[["src"]] <- paste(new.output.folder,"/merged_",plot_name,"_tile-",i,".vox", sep="")
  
  n.scans=length(root[["process"]][["input"]][["scans"]]) ##Get the number of scans in the project
  for (j in 1:n.scans){
    
    if (j<10){scan.pos=paste("00",j, sep="")}else if(j>9 & j<100) {scan.pos=paste("0",j, sep="")} else{scan.pos=j}
    
    old.scan.file=xmlAttrs(root[["process"]][["input"]][["scans"]][j]$scan)[["src"]]
    scan.name=unlist(strsplit(old.scan.file[1],"[\\\\]" ))
    scan.name=scan.name[length(scan.name)]
    scan.pos=unlist(strsplit(old.scan.file[1],"[\\\\]" ))
    scan.pos=scan.pos[length(scan.pos)-2]
    
    new.scan.file=paste(new.scan.folder,scan.pos,"/SINGLESCANS/",scan.name, sep="")
    xmlAttrs(root[["process"]][["input"]][["scans"]][j]$scan)[["src"]] <- new.scan.file
    
  }
  ###########################################################################
  ###Change DTM folder#####
  xmlAttrs( root[["process"]][["filters"]][["dtm-filter"]])[["src"]] <- new.dtm.folder
  
  ###########################################################################  
  out.name=paste("config_",plot_name,"_tile-",i,".xml", sep="") 
  saveXML(root, file=out.name)
  
}

##############################################
#########CALCULATE PAI########################
##############################################

# specify library paths (puhti)
#.libPaths(c("/projappl/project_2001208/1-Permanent_files/R_Packages/", .libPaths()))

# required libraries
#install.packages('plotfunctions')
#install.packages('fANCOVA')
#install.packages("plotrix")
#install.packages("ggplot2")
#install.packages("ggthemes")
require(ggthemes)
require(ggplot2)
library(plotrix)
require(plotfunctions)
require(raster)
require(fANCOVA)
require(rgeos)
require(dplyr)

# remove environment (optional)
rm(list=ls())

# set voxel size and pad.coefficients
voxel.size=1
pad.coefficient=0.5 #https://doi.org/10.1016/j.rse.2018.06.024

# metric output folder
# CHANGE
metric.out.folder=paste("E:/winSCP/TLS_AMAPVox_Processing/Metric_tables")

############################################################################################
# plot name
# CHANGE
plot_name="las_17"

###set folders
# CHANGE
location.folder=paste("E:/registered_data/iris-17.RiSCAN")
vox.files.folder=paste("E:/winSCP/TLS_AMAPVox_Processing/",plot_name,"/",plot_name,"_output_voxel_files/", sep="")

###read CHM
# CHANGE
chm.file=paste("E:/las_data_new/las_17/las_17_dem/CHM_",plot_name,".tif", sep="")
chm=raster(chm.file)

###Get scan locations and create a buffer around them
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
unique.xy.sp=SpatialPoints(unique.xy)

m=cbind(all.x, all.y, all.k, all.gdist, all.pad)
m=m[complete.cases(m),] #remove NA = below ground voxels

m[m[,"all.gdist"]<=0.5,]<-NA
m=m[complete.cases(m),] #remove NA = below 0.5

# export raw PAD values as matrix
write.csv(m, paste("PAD_DF_untrimmed/", plot_name, "_untrimmed.csv", sep = ""))

# convert matrix to df
pad.df <- as.data.frame(m)
plot(pad.df$all.gdist ~ pad.df$all.pad)
# subset to get rid of height outliers, find better way
# unnecessary since we cancel out everything thats not 0 anyways
# pad.df.subset <- subset(pad.df, all.gdist < 30)
# subset pad values above > 0 
pad.df.subset_two <-subset(pad.df, all.pad > 0)
# scatterplot PAD versus ground distance
plot(pad.df.subset_two$all.gdist ~ pad.df.subset_two$all.pad)
pad.df <- pad.df.subset_two
# density plot for subset two
write.csv(pad.df, paste("PAD_DF/", plot_name, "_trimmed.csv", sep = ""))

# LETS see if theres a better way
#pad.df$gdist <- 
  #pad.df %>% mutate(
    #equal_gdist = case_when(all.gdist <= 1.0 ~ 1,
    #                                       (all.gdist > 1.0 & all.gdist <= 2) ~ 2)
    #)
# categorize into 20 1er steps, maybe this must be reviewed for a better breaks
pad.df <- pad.df %>% mutate(gdist_equal = cut(all.gdist, 
                                              breaks = 0:57, 
                                              labels = 1:57,
                                              include.lowest = TRUE)
                            )
# summarize and calculate metrics using dplyr 
pad.stat <- pad.df %>%
  group_by(gdist_equal) %>%
  summarise_at(vars(all.pad), list(mean = mean, 
                                   sd = sd, 
                                   se = std.error, 
                                   n = length,
                                   med = median,
                                   max = max,
                                   min = min
                                   )
  )
# get rid of stinky factor
pad.stat$mean <- as.numeric(pad.stat$mean)
pad.stat$gdist_equal <- as.numeric(pad.stat$gdist_equal)
pad.stat$sd <- as.numeric(pad.stat$sd)
pad.stat$se <- as.numeric(pad.stat$se)
pad.stat$n <- as.numeric(pad.stat$n)
pad.stat$med <- as.numeric(pad.stat$med)
pad.stat$max <- as.numeric(pad.stat$max)
pad.stat$min <- as.numeric(pad.stat$min)
# round standard error
pad.stat$se_round <- round(pad.stat$se,3)
# CI interval
pad.stat$CI <- pad.stat$se*1.96
pad.stat$CI_upper <- pad.stat$mean + pad.stat$CI
pad.stat$CI_lower <- pad.stat$mean - pad.stat$CI
pad.stat$plot_name <- "iris-17"

# export summary stats
write.csv(pad.stat, paste("Summary_Metrics_DF/", plot_name, "_pad_stats.csv", sep = ""))

# plot name
plot_name = "las_13"
# import summary stats
pad.stat <- read.csv(paste("Summary_Metrics_DF/", plot_name, "_pad_stats.csv", sep = ""))
# remove empty rows OPTIONAL
pad.stat <- pad.stat[-c(17:56),]

############################
#######BASE R PLOT##########
############################
par(bg = "#f7f7f7")
plot(pad.stat$mean, 
     pad.stat$gdist_equal,
     ylim=c(1,20),
     xlim=c(0, 1),
     pch = 19,
     cex = 1.2,
     axes= FALSE,
     type = "o",
     xlab = "Plant Area Density (PAD) m2 m-3",
     ylab = "Height Above Ground [m]",
     main = "Botanical Garden (Iris-1)",
     sub = "n = 11065")
# bounding box of CI, skip when parsin
polygon(x = c(pad.stat$CI_lower, rev(pad.stat$CI_upper)),
        y = c(pad.stat$gdist_equal, rev(pad.stat$gdist_equal)),
        lty=1,
        col= adjustcolor("orangered", alpha.f=0.1)
)
# enable x axis
axis(side=1, at=c(0, 0.2, 0.4, 0.6, 0.8, 1))
# enable y axis
axis(side=2, at=c(1:20))
arrows(pad.stat$mean - pad.stat$CI, 
       pad.stat$gdist_equal,
       pad.stat$mean + pad.stat$CI, 
       pad.stat$gdist_equal,
       code = 0, length= 0.03, angle = 90)
# n = number of samples
#text(3,
     #pad.stat$gdist_equal,
     #labels = as.character(pad.stat$n),
     #adj = 1)
#text(2.6,
     #20,
     #labels = "n =",
     #adj = 0,
     #cex = 1)
# add standard error
text(1,
     pad.stat$gdist_equal,
     labels = as.character(pad.stat$se_round),
     adj = 1,
     col = "#808080",
     cex = 0.8)
# add confidence interval
lines(pad.stat$CI_lower,
      pad.stat$gdist_equal,
      col = "#bd0026")
lines(pad.stat$CI_upper,
      pad.stat$gdist_equal,
      col = "#bd0026")
# add vertical and horizontal grid
axis(1, tck = 1, lty = 2, col = "gray")
axis(2, tck = 1, lty = 2, col = "gray")
# parse new, run everything up to grid afterwards function afterwards
par(new = TRUE)


############################
#######  GGPLOT2  ##########
############################
ggplot(data = pad.stat, aes(x=mean, y=gdist_equal)) +
  geom_ribbon(aes(xmin = CI_lower, xmax = CI_upper, 
                  fill = "CI95"), alpha = 0.2)+
  geom_ribbon(aes(xmin = mean-se, xmax = mean+se, 
                  fill = "Std. Err."), alpha = 0.5)+
  geom_path(size = 0.7)+
  geom_point(size = 2)+
  scale_x_continuous( "(Mean) Plant Area Density (PAD) m2 m-3", 
                      limits=c(-0.1,2), 
                      breaks = seq(0, 2, 0.2))+
  scale_y_continuous( "Height above Ground (m)", 
                      limits=c(1,30),
                      breaks = seq(0, 30, 4))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "17: Ngerenyi Campus (Las-17)",
       subtitle = "n = 13472",
       caption = "",
       fill = "")+
  scale_fill_manual(values=c("#0C7BDC", "#FFC20A"))

# save plot as pdf and png
ggsave(paste("images/Mean_PAD/ggplot2/", plot_name, "_pda_gg.pdf", sep = ""),
       width = 15,
       height = 20,
       units = "cm")
ggsave(paste("images/Mean_PAD/ggplot2/", plot_name, "_pda_gg.png", sep = ""),
       width = 15,
       height = 20,
       units = "cm")
############################
#######  MAX MIN MEd #######
############################
# for later

# test for normal distribtuion (probably irrelevant)
shapiro.test(pad.stat$mean)
# this is irrelevant too
lm.fit <- lm(pad.df.subset$all.pad ~ pad.df.subset$all.gdist)
abline(lm.fit, col = "red")
# correlation test, also pretty unsignificant
res <- cor.test(pad.df.subset$all.gdist,
                pad.df.subset$all.pad,
                method = "kendall")
res

#####################################
##location of voxels
plot(chm)
plot(unique.xy.sp, add=T)
###################################################################
###Calculate PAI for each column 
pai=numeric()
for (i in 1:nrow(unique.xy)){
  
  v.col=unique.xy[i,]
  
  m.sub=m[m[,1]==v.col[1] & m[,2]==v.col[2], ]
  
  PAI.col=sum(m.sub[,"all.pad"], na.rm=T)
  pai=rbind(pai, c(v.col, PAI.col))
  
}

r.pai=rasterFromXYZ(pai, res=c(1,1), crs="", digits=5)

################################################################### 
# what do we want from this?

writeRaster(r.pai, "/scratch/project_2001208/FIRSTNAME/las_data/TLS_AMAPVox_Processing/Metric_tables/las_1_pai_raster.tif")




  