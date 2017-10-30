library(raster)
library(ggplot2)
library(gridExtra)

## Import data
# GBIF Records
AllRecs<-read.csv("AlliariaLocsGBIF.csv",header=T) # NOTE: Occurrence data from GBIF.ORG
AllRecs<-AllRecs[AllRecs$decimalLatitude > 26,] # Exclude tropics and southern hemisphere
# Add region info for later graphing
AllRecs$Region<-NA
AllRecs$Region[AllRecs$decimalLongitude > -35 & AllRecs$decimalLongitude < 50]<-"Europe" # Europe, western Asia and Middle East
AllRecs$Region[AllRecs$decimalLongitude < -35 & AllRecs$decimalLongitude  > -168]<-"NorthAm" # North American Longitudes
# Check that locations are only in North America and Europe
qplot(AllRecs$decimalLongitude,AllRecs$decimalLatitude)+ylim(-90,90)+xlim(-180,180) 
# GGMFS Data
PopMeans<-read.csv("Population_Means.csv", header=T) # Import data
PopLocs<-PopMeans[,c("Longitude","Latitude","Pop_Code","Region")] # Remove unused variables

# Loadings from Climate Model (PCRegression)
Load<-read.csv("ClimLoadings.csv",row.names=1)
# Import climate data
Files <- list.files(path=paste(getwd(),"climond_data","CM10_1975H_Bio_ESRI_V12",sep="/"),pattern='w001001.adf', recursive=TRUE,full.names=TRUE )
Predictors <- stack(Files)
names(Predictors)<-paste0("bio",c(1:length(names(Predictors))))

#-----#
#!# Extract bioclim data from PopLocs locations
## Separate graphs for Europe and North Am
LongNA<-c(-1350:-500)/10
LatNA<-c(300:550)/10
coordsNA<-cbind(rep(LongNA,length(LatNA)),sort(rep(LatNA,length(LongNA))))
LongEU<-c(-100:500)/10
LatEU<-c(300:650)/10
coordsEU<-cbind(rep(LongEU,length(LatEU)),sort(rep(LatEU,length(LongEU))))
coords<-rbind(coordsNA,coordsEU)
PredMap <- data.frame(extract(Predictors, coords))
# Remove unused bioclim variables
PredMap<-PredMap[,names(PredMap) %in% row.names(Load)]
# Climate predictions for each response variable
FruitPred<-apply(PredMap,1,function(x) t(sum(x*t(Load$Fruits))))
TotalDensPred<-apply(PredMap,1,function(x) t(sum(x*t(Load$TotalDens))))
RosRatioPred<-apply(PredMap,1,function(x) t(sum(x*t(Load$RosRatio))))
PopSizePred<-apply(PredMap,1,function(x) t(sum(x*t(Load$PopSize))))

# 'Threat' calculations
ThreatPred<-apply(PredMap,1,function(x) t(sum(x*t(Load$Fruits))+sum(x*t(Load$TotalDens))+sum(x*t(Load$PopSize))))
PopMeans$PerfInd<-log(PopMeans$Fruits+1)+log(PopMeans$Pop_Size+1)+log(PopMeans$TotalDens+1)

# Set heatmap colours - should be list of 5 colours for min, 25%, 50%, 75% and max
Clrs<-c("black","blue","cyan","green","yellow","red","white")

## Plot Europe and NorthAm on same Map
## Invasion Threat
plotmap<-function(pred=NA,title="NONE",mega=NA){
  # Subset Data for Plotting
  PData<-data.frame(X=coords[,1],Y=coords[,2],Z=pred)
  PData$Region[PData$X > -35]<-"Europe" # Europe, western Asia and Middle East
  PData$Region[PData$X < -35]<-"NorthAm" # North American Longitudes
  # Identify "Mega-Populations" (highest 95% of vector defined by 'mega' parameter)
  MegaPops<-PopMeans[PopMeans[,mega]>quantile(PopMeans[,mega], 0.95,na.rm=T),]
  MegaPops<-MegaPops[!is.na(MegaPops[,mega]),]
  # Create plot
  PltMap<-ggplot(aes(x=X,y=Y),data=PData)+theme_classic(base_size=12)+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs)+
    geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopLocs,size=2,shape=21)+
    geom_point(aes(x=Longitude,y=Latitude),data=MegaPops,colour="black",fill="#ff80e5ff",size=3,shape=24)+
    ggtitle(title)
  # Output plot to .png
    return(PltMap)
}
png("Maps/Predict_Fruits.png",width=21,height=7,units="in",res=200)
  plotmap(pred=FruitPred,title="Predicted Fruits",mega="Fruits")
dev.off()
png("Maps/Predict_Density.png",width=21,height=7,units="in",res=200)
  plotmap(pred=TotalDensPred,title="Predicted Density",mega="TotalDens")
dev.off()
png("Maps/Predict_RosRatio.png",width=21,height=7,units="in",res=200)
  plotmap(pred=RosRatioPred,title="Predicted Rosette Ratio",mega="RosRatio")  
dev.off()
png("Maps/Predict_PopSize.png",width=21,height=7,units="in",res=200)
  plotmap(pred=PopSizePred,title="Predicted Pop Size",mega=Pop_Size)
dev.off()
png("Maps/Predict_Threat.png"),width=21,height=7,units="in",res=200)
  plotmap(pred=ThreatPred,title="Predicted Threat",mega=PerfInd)
dev.off()



## Fruits
PData$Z<-FruitPred
MegaPops<-PopMeans[PopMeans$Fruits>quantile(PopMeans$Fruits, 0.95,na.rm=T),]
MegaPops<-MegaPops[!is.na(MegaPops$Fruits),]
PltMap2<-ggplot(aes(x=X,y=Y),data=PData)+theme_classic(base_size=12)+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs)+
  geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopLocs,size=2,shape=21)+
  geom_point(aes(x=Longitude,y=Latitude),data=MegaPops,colour="black",fill="#ff80e5ff",size=3,shape=24)+
  ggtitle("Average Fruit Number")

png("Maps/Predicted_Fruits.png",width=21,height=7,units="in",res=200)
  PltMap2
dev.off()

## Density
PData$Z<-TotalDensPred
MegaPops<-PopMeans[PopMeans$TotalDens>quantile(PopMeans$TotalDens, 0.95,na.rm=T),]
MegaPops<-MegaPops[!is.na(MegaPops$TotalDens),]
PltMap3<-ggplot(aes(x=X,y=Y),data=PData)+theme_classic(base_size=12)+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs)+
  geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopLocs,size=2,shape=21)+
  geom_point(aes(x=Longitude,y=Latitude),data=MegaPops,colour="black",fill="#ff80e5ff",size=3,shape=24)+
  ggtitle("Average Density")

png("Maps/Predicted_Density.png",width=21,height=7,units="in",res=200)
  PltMap3
dev.off()

## Rosette Ratio
PData$Z<-RosRatioPred
MegaPops<-PopMeans[PopMeans$RosRatio>quantile(PopMeans$RosRatio, 0.95,na.rm=T),]
MegaPops<-MegaPops[!is.na(MegaPops$RosRatio),]
PltMap4<-ggplot(aes(x=X,y=Y),data=PData)+theme_classic(base_size=12)+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs)+
  geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopLocs,size=2,shape=21)+
  geom_point(aes(x=Longitude,y=Latitude),data=MegaPops,colour="black",fill="#ff80e5ff",size=3,shape=24)+
  ggtitle("Rosette Ratio")

png("Maps/Predicted_RosRatio.png",width=21,height=7,units="in",res=200)
  PltMap4
dev.off()


## Separate maps
#PltNA<-ggplot(aes(x=X,y=Y),data=PData[PData$Region=="NorthAm",])+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs)+
#  geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopLocs[PopLocs$Region=="NorthAm",],size=2,shape=21)+
#  ggtitle("Invasion Threat")+ theme_classic(base_size=12)

#PltEU<-ggplot(aes(x=X,y=Y),data=PData[PData$Region=="Europe",])+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs)+
#  geom_point(aes(x=Longitude,y=Latitude),colour="black",fill="white",data=PopLocs[PopLocs$Region=="Europe",],size=2,shape=21)+
#  ggtitle("Invasion Threat")+ theme_classic(base_size=12)

#png("Maps/Predicted_Threat.png",width=21,height=7,units="in",res=200)
#  grid.arrange(PltNA,PltEU,nrow=1)
#dev.off()



#-----#
#!# Create data for plotting
PlotData<-cbind(PopLocs,data.frame(PresVals))
PlotDist<-cbind(AllRecs,data.frame(DistVals))
PlotDist<-na.omit(PlotDist)