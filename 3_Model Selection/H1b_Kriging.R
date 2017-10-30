############################################
## This script is for kriging fitness variables to look for spatial structure

##############################
## Load functions
##############################
#library(geoR)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(rgdal)
library(automap)
##############################
## Spatial kriging function
##############################
source("Functions/krig.R")

##############################
## Load data
##############################
PopData<-read.csv("Population_Means_wClimate_wGDD.csv")

##############################
## Function to Plot Kriging output
##############################
pkrig<-function(Data=PopData,Var=NA,Region=NA,LogT=T,Area=25){
  # Set heatmap colours - should be list of 5 colours for min, 25%, 50%, 75% and max
  Clrs<-c("black","blue","cyan","green","yellow","red","white")
  Krig<-krig(Data=Data,Var=Var,Region=Region,LogT=LogT,Area=Area)
  # Extract and convert to dataframe for plotting
  plot(Krig) # Quick-view of Kriging Map
  # Subset for plotting
  KrigData<-as.data.frame(Krig$krige_output)[,1:3]
  # Back-transform to lat/long
  names(KrigData)<-c("long","lat","Pred")
  coordinates(KrigData)=~long+lat
  proj4string(KrigData)<-"+proj=merc +zone=18s +ellps=WGS84 +datum=WGS84" # Define current projection
  KrigData<-spTransform(KrigData, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))  # back-transform to long/lat
  PDat<-as.data.frame(KrigData)
  name<-Var
  # Set limits of heat map colours based on 2.5% and 97.5% of values
  # Do it for all data from both regions to compare across maps
  lims<-c(min(Data[!is.na(Data[,Var]),Var]),
          sort(Data[!is.na(Data[,Var]),Var])[c(round(length(Data[,Var])*0.17,0),round(length(Data[,Var])*0.33,0),round(length(Data[,Var])*0.5,0),round(length(Data[,Var])*0.67,0),round(length(Data[,Var])*0.83,0))],
          max(Data[!is.na(Data[,Var]),Var]))
  lims<-lims-min(lims)
  if(LogT==T){
    name<-paste("Log",Var)
    lims<-log(lims+1)
  }
  if(Region=="NorthAm"){
    Plt<-BaseMapNA
  }
  if(Region=="Europe"){
    Plt<-BaseMapEU
  }
  # Create plot
  Plt<-Plt+geom_point(aes(x=long,y=lat,colour=Pred),data=PDat,shape=15,alpha=1,size=2)+scale_color_gradientn(name=name,colours=Clrs,values=lims/max(lims))+
    geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopData[!is.na(PopData[,Var]),],size=2,shape=21)+ggtitle(name)
  return(Plt)
}
# Import Maps of Europe and NorthAm if they haven't already been imported
if(!exists("BaseMapNA")){
  BaseMapNA<-ggmap(get_map(location=c(-96,40),zoom=3,source="google",maptype="satellite",color="bw"))+xlim(-125,-55)+ylim(20,60)+
    geom_polygon(x=c(-150,-150,-50,-50),y=c(15,65,65,15),fill="white",alpha=0.5)
}
if(!exists("BaseMapEU")){
  BaseMapEU<-ggmap(get_map(location=c(17,50),zoom=3,source="google",maptype="satellite",color="bw"))+xlim(-20,50)+ylim(25,65)+
    geom_polygon(x=c(-40,-40,70,70),y=c(10,80,80,10),fill="white",alpha=0.5)
}
# Set radius size for kriging around sampling locations (Use Area=0 for convex hull)
Area=30

### Begin mapping
## Fruits/StdzFruits
FruitsNA<-pkrig(Var="Fruits",Region="NorthAm",LogT=T,Area=Area)
FruitsEU<-pkrig(Var="Fruits",Region="Europe",LogT=T,Area=Area)
StdzFruitsNA<-pkrig(Var="StdzFruits",Region="NorthAm",LogT=T,Area=Area)
StdzFruitsEU<-pkrig(Var="StdzFruits",Region="Europe",LogT=T,Area=Area)
# Save as multi-panel plots
png("Maps/KrigFruits.png",width=21,height=14,units="in",res=200,pointsize=0)
  grid.arrange(FruitsNA,FruitsEU,StdzFruitsNA,StdzFruitsEU)
dev.off()

## RosRatio/Total Size
RosRNA<-pkrig(Var="RosRatio",Region="NorthAm",LogT=F,Area=Area)
RosREU<-pkrig(Var="RosRatio",Region="Europe",LogT=F,Area=Area)
TSizeNA<-pkrig(Var="TotalSize",Region="NorthAm",LogT=T,Area=Area)
TSizeEU<-pkrig(Var="TotalSize",Region="Europe",LogT=T,Area=Area)
# Save as multi-panel plots
png("Maps/KrigPopSize.png",width=21,height=14,units="in",res=200,pointsize=0)
  grid.arrange(RosRNA,RosREU,TSizeNA,TSizeEU)
dev.off()

## Individual size measurements
HeightNA<-pkrig(Var="Height",Region="NorthAm",LogT=F,Area=Area)
HeightEU<-pkrig(Var="Height",Region="Europe",LogT=F,Area=Area)
StdzHeightNA<-pkrig(Var="StdzHeight",Region="NorthAm",LogT=F,Area=Area)
StdzHeightEU<-pkrig(Var="StdzHeight",Region="Europe",LogT=F,Area=Area)
# Save as multi-panel plots
png("Maps/KrigPlantSize.png",width=21,height=14,units="in",res=200,pointsize=0)
  grid.arrange(HeightNA,HeightEU,StdzHeightNA,StdzHeightEU)
dev.off()

## Densities
RosDensNA<-pkrig(Var="RosDens",Region="NorthAm",LogT=T,Area=Area)
RosDensEU<-pkrig(Var="RosDens",Region="Europe",LogT=T,Area=Area)
AdultDensNA<-pkrig(Var="AdultDens",Region="NorthAm",LogT=T,Area=Area)
AdultDensEU<-pkrig(Var="AdultDens",Region="Europe",LogT=T,Area=Area)
# Save as multi-panel plots
png("Maps/KrigDens.png",width=21,height=14,units="in",res=200,pointsize=0)
  grid.arrange(RosDensNA,RosDensEU,AdultDensNA,AdultDensEU)
dev.off()








