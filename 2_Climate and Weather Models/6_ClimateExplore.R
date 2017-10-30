#########################################################
### This script explores and maps climatic variation, ###
### focusing on WorldClim bioclim variables           ###
#########################################################

library(maptools)
library(rgbif)
library(dismo)
library(rgdal)
library(ggplot2)

#####################################################################################################
### NOTE: download bioclim data from climond.org; data in ERSI grids can be used with 'raster' in R
### These should be saved in a new folder called: "../climond_data"
#####################################################################################################

##############################
## Load custom FUNCTIONS
##############################
# Climate maps
source("Functions/climapper.R")
# Basic PCA of bioclim variables
source("Functions/bioPCA.R")

#########################
## Data import and setup
#########################
## Input the raster files
## NB: This 'files' command has to match the file directory for the bioclim data
Files <- list.files(path=paste(getwd(),"climond_data","CM10_1975H_Bio_ESRI_V12",sep="/"),pattern='w001001.adf', recursive=TRUE,full.names=TRUE )
Predictors <- stack(Files)
names(Predictors)<-paste0("bio",c(1:length(names(Predictors))))
### NB: Bio36-40 are first 5 PCs of the other bioclim variablers

## Import data from gbif.org (or load if already present)
if(!file.exists("GBIF_Locations.csv")){  
  ApKey<-name_suggest(q='Alliaria petiolata', rank='species')$key[1]
  ApData<-occ_search(taxonKey=ApKey,limit=199000)
  write.csv(ApData$data,"GBIF_Locations.csv",row.names=F)
}
ApLocs<-read.csv("GBIF_Locations.csv")[,c("decimalLatitude","decimalLongitude")]
# Extract lat/long coordinates
AllRecs<-data.frame(Latitude=ApLocs$decimalLatitude,Longitude=ApLocs$decimalLongitude,Region=NA)
# Add region info for later graphing
AllRecs$Region[AllRecs$Longitude > -35 & AllRecs$Longitude < 50]<-"Europe" # Europe, western Asia and Middle East
AllRecs$Region[AllRecs$Longitude < -35 & AllRecs$Longitude  > -168]<-"NorthAm" # North American Longitudes
# Exclude tropics and southern hemisphere
AllRecs<-AllRecs[AllRecs$Latitude > 26,] 
# Remove missing coordinates
AllRecs<-AllRecs[complete.cases(AllRecs),]
# Check that locations are only in North America and Europe
qplot(AllRecs$Longitude,AllRecs$Latitude)+ylim(-90,90)+xlim(-180,180) 

PopMeans<-read.csv("Population_Means.csv", header=T) # Import data
PopLocs<-PopMeans[,c("Longitude","Latitude","Pop_Code","Region")] # Remove unused variables

#########################
## Plot the worldclim data and GM sample points
#########################
windows(width=16.5, height=5.5, rescale="fixed") # Use this to plot in separate window  
# Plot map with bioclim overlay
plot(Predictors$bio36,xlim=c(-125,45),ylim=c(30,60),1,main="",xaxt="n",yaxt="n",legend=F) ## NB: Need wide, narrow plot window to get best pic
AllSites<-AllRecs[,c("Longitude","Latitude")]
points(AllSites, col="#F4007633", cex=0.1,pch=16) #GBIF pop locations  
PopLocsRnd<-PopLocs[,c("Longitude","Latitude")]+runif(382,-0.2,0.2) # add very slight random noise to better see points that are really close together
points(PopLocsRnd, col="#003F91FF", cex=1.5,pch=21) #GGMFS pop locations
points(PopLocsRnd, col="#98B9F266", cex=1,pch=16) #GGMFS pop locations 

#########################
## Extract data from bioclim variables
#########################
### Extract bioclim data from PopLocs locations
PresVals <- extract(Predictors, PopLocs[,c("Longitude","Latitude")])
DistVals <- extract(Predictors, AllRecs[,c("Longitude","Latitude")])
# Problem: Data not available for a few locations
NoDat<-!complete.cases(PresVals)
# In these cases, average of closest locations
PresVals[NoDat,]<-extract(Predictors, PopLocs[NoDat,c("Longitude","Latitude")],buffer=100000,fun=mean)

########################
## Plot populations in climate space
#########################
### GGMFS SAMPLE POPULATIONS
## Basic PCA of climate data:
PlotData<-cbind(PopLocs,data.frame(PresVals))
# Add PCs 
PlotData<-cbind(PlotData,bioPCA(PlotData))

### GBIF OCCURRENCE DATA
PlotDist<-cbind(AllRecs,data.frame(DistVals))
PlotDist<-PlotDist[complete.cases(PlotDist),]
# Add PCs 
PlotDist<-cbind(PlotDist,bioPCA(PlotDist))

### Base plot
p <- ggplot() + xlab("PC1") + ylab("PC2") + theme_classic(base_size=48) + guides(fill=FALSE)
# Optional: Add Convex hulls
#p <- p + geom_polygon(data = ConvHullNat,aes(x=PC1,y=PC2),fill="#4FB0C666", alpha = 0.2)
#p <- p + geom_polygon(data = ConvHullInt,aes(x=PC1,y=PC2),fill="#F5375166", alpha = 0.2)

### Plot for field survey locations
p1 <- p + geom_point(data=PlotData[PlotData$Region=="Europe",],aes(x=PC1,y=PC2),colour="#4FB0C633",size=8,shape=16) 
p1 <- p1 + geom_point(data=PlotData[PlotData$Region=="Europe",],aes(x=PC1,y=PC2),colour="#4FB0C6FF",size=8,shape=1) 
p1 <- p1 + geom_point(data=PlotData[PlotData$Region=="NorthAm",],aes(x=PC1,y=PC2),colour="#F5375133",size=8,shape=16)
p1 <- p1 + geom_point(data=PlotData[PlotData$Region=="NorthAm",],aes(x=PC1,y=PC2),colour="#F53751FF",size=8,shape=1)
png("DataExplore/Climates_ByPop.png",width=21,height=14,units="in",res=200)
  p1
dev.off()

### Plot for GBIF records
p2 <- p + geom_point(data=PlotDist[PlotDist$Region=="Europe",],aes(x=PC1,y=PC2),colour="#4FB0C633",size=3,shape=1)
p2 <- p2 + geom_point(data=PlotDist[PlotDist$Region=="NorthAm",],aes(x=PC1,y=PC2),colour="#F5375133",size=3,shape=1)
png("DataExplore/Climates_DistOnly.png",width=21,height=14,units="in",res=200)
  p2
dev.off()

#################
### Climate Maps
#################
### Make Map Directory if needed
dir.create("Maps", showWarnings = FALSE)
# Map output to new window for immediate inspection
climapper(Type="win",Locs=T,Borders=T)
#!# Climate graphs with different options # NOTE: These are saved directly to "Maps" folder
climapper(Type="png")
climapper(Type="png",Borders=T)
climapper(Type="png",Locs=T)
climapper(Type="png",Borders=T,Locs=T)
climapper(Type="png",Borders=T,Locs=T,AllLocs=T)

#################
### Output data with bioclim variables
#################
PopData<-merge(PopMeans,PlotData[,grep("bio|Pop_Code",names(PlotData))],by="Pop_Code")
write.csv(PopData,"Population_Means_wClimate.csv", row.names=FALSE) 
