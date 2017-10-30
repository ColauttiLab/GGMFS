#########################
## This script plots performance of sample locations onto climate map
#########################

#########################
## Libraries
#########################

library(raster)
library(ggplot2)
library(gridExtra)
source("Functions/theme_map.R")

#########################
## Import data
#########################

# GBIF Records
AllRecs<-read.csv("GBIF_Locations.csv",header=T) # NOTE: Occurrence data from GBIF.ORG
AllRecs<-AllRecs[AllRecs$decimalLatitude > 26,] # Exclude tropics and southern hemisphere
# Add region info for later graphing
AllRecs$Region<-NA
AllRecs$Region[AllRecs$decimalLongitude > -35 & AllRecs$decimalLongitude < 50]<-"Europe" # Europe, western Asia and Middle East
AllRecs$Region[AllRecs$decimalLongitude < -35 & AllRecs$decimalLongitude  > -168]<-"NorthAm" # North American Longitudes
# Check that locations are only in North America and Europe
qplot(AllRecs$decimalLongitude,AllRecs$decimalLatitude)+ylim(-90,90)+xlim(-180,180) 
# GGMFS Data
PopMeans<-read.csv("PCData.csv", header=T) # Import data
PopLocs<-PopMeans[,c("Longitude","Latitude","Pop_Code","Region")] # Remove unused variables

# Loadings from Climate Model (PCRegression)
Load<-read.csv("ClimLoadings.csv",row.names=1)
# Import climate data
Files <- list.files(path=paste(getwd(),"climond_data","CM10_1975H_Bio_ESRI_V12",sep="/"),pattern='w001001.adf', recursive=TRUE,full.names=TRUE )
Predictors <- stack(Files)
names(Predictors)<-paste0("bio",c(1:length(names(Predictors))))


#########################
## Extract bioclim data for mapping
#########################
## Separate graphs for Europe and North Am
# Set map boundaries
LongNA<-c(-1300:-500)/10
LatNA<-c(320:600)/10
coordsNA<-cbind(rep(LongNA,length(LatNA)),sort(rep(LatNA,length(LongNA))))
LongEU<-c(-100:450)/10
LatEU<-c(320:600)/10
coordsEU<-cbind(rep(LongEU,length(LatEU)),sort(rep(LatEU,length(LongEU))))
coords<-rbind(coordsNA,coordsEU)
# Delete to save memory
coordsEU<-coordsNA<-0
# Extract bioclim variables for x-y grid within boundaries
PredMap<-data.frame(extract(Predictors, coords)[,c(-3,-7,-36:-40)])# Keep only bioCLim variables used in PCregession model (i.e. not 3,7,36:40
# Standardize each bioclim variable to mean=0 sd=1
PredMap<-data.frame(scale(PredMap))
colMeans(PredMap,na.rm=T) # Check means
apply(PredMap,2,sd,na.rm=T) # Check SD

# Extract PC1 from bioclim data
Load<-princomp(PopMeans[,grep("bio",names(PopMeans))],cor=T)$loadings[,1]
cbind(names(PredMap),row.names(Load)) # Make sure names match
# Make map of predicted PC1 for all of NorthAm and Europe
bioPC1<-apply(PredMap,1,function(x) sum(x*Load))

## Plot Europe and NorthAm on same Map
## Invasion Threat
plotmap<-function(pred=NA,title=mega,mega=NA,mname=mega,logt=T,pct95=T){
  # Subset Data for Plotting
  PData<-data.frame(X=coords[,1],Y=coords[,2],Z=pred)
  PData$Region[PData$X > -35]<-"Europe" # Europe, western Asia and Middle East
  PData$Region[PData$X < -35]<-"NorthAm" # North American Longitudes
  # Choose variable for circle size
  Perf<-PopMeans[,c("Longitude","Latitude",mega)]
  names(Perf)[3]<-"mega"
  if(pct95==T){
    # Identify "Mega-Populations" (highest 95% of vector defined by 'mega' parameter)
    ## Define mega-pops based on percentile
    MegaPops<-PopMeans[PopMeans[,mega]>quantile(PopMeans[,mega], 0.95,na.rm=T),]
  }
  if(pct95==F){
    # Define mega-pops based on threshold value
    MegaPops<-PopMeans[PopMeans[,mega]>1000000,]
  }
  MegaPops<-MegaPops[!is.na(MegaPops[,mega]),]
  # Create plot
  PltMap<-ggplot(aes(x=X,y=Y),data=PData)+theme_map()+geom_point(aes(colour=Z),shape=15,alpha=1,size=1)+scale_color_gradientn(colours=Clrs,na.value=Ocn,guide=F)+
    geom_point(aes(x=Longitude,y=Latitude),colour="black",data=PopMeans,shape=16,size=1)
  if(logt==T){PltMap<-PltMap+geom_point(aes(x=Longitude,y=Latitude,size=log(mega+1)),data=Perf,colour=NA,fill=HFil,alpha=0.3,shape=21)+
                  geom_point(aes(x=Longitude,y=Latitude,size=log(mega+1)),data=Perf,colour=HClr,fill=NA,shape=21)}
  if(logt==F){PltMap<-PltMap+geom_point(aes(x=Longitude,y=Latitude,size=mega),data=Perf,colour=NA,fill=HFil,alpha=0.3,shape=21)+
                geom_point(aes(x=Longitude,y=Latitude,size=mega),data=Perf,colour=HClr,fill=NA,shape=21)}  
  PltMap<-PltMap+geom_point(aes(x=Longitude,y=Latitude),colour=MClr,data=MegaPops,shape=16,size=3)+
    ggtitle(title)+scale_size(range = c(0, 15),name=mname)
  print(paste0("Native: N =",length(PopMeans$Region[PopMeans$Region=="Europe"]),"; Introduced: N =",length(PopMeans$Region[PopMeans$Region=="NorthAm"])))
  print(paste0("Native: Mean =",median(PopMeans[PopMeans$Region=="Europe",mega],na.rm=T),"; Introduced: Mean =",median(PopMeans[PopMeans$Region=="NorthAm",mega],na.rm=T)))
  print(paste0("lowest megas:",paste(sort(MegaPops[,mega])[1:3],collapse=" ")))
  print(paste0("highest megas:",paste(sort(MegaPops[,mega],decreasing=T)[1:3],collapse=" ")))
  print(paste0("N megas:",length(MegaPops[,mega])))
  print(paste0("megas as % of all sites:",round(100*length(MegaPops[,mega])/sum(!is.na(PopMeans[,mega])),digits=3),"%"))
  # Output plot to .png
  return(PltMap)
}

# Set heatmap colours (light,med,dark works best)
MClr<-"white" # Highlight colour for 'megapopulations'
Ocn<-"white" # Ocean Colour

## Colour Scheme A
Clrs<-c("#2C365E","#C4E0F9") # Scales for olour gradient map (note: can use 3+ colours)
HClr<-"#4C191B" # Colour of circle outlines
HFil<-"#963D5A" # Circle fill colour

# Total N individuals
png("4_Figures/Fig1_TotalSize.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,mega="TotalSize",mname="log(N)",title="Total N individuals",pct95=F)
dev.off()
# List names of 'mega-populations' (to search for pics)
PopMeans[PopMeans$TotalSize>=1000000 & !is.na(PopMeans$TotalSize),c("Pop_Code","TotalDens","TotalSize","Year","Family_Name","Given_Name","email")]
# Or high density
PopMeans[PopMeans$TotalDens>=250 & !is.na(PopMeans$TotalSize),c("Pop_Code","TotalDens","TotalSize","Year","Family_Name","Given_Name","email")]


# Density of all individuals
png("4_Figures/Fig1_TotalDens.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,mega="TotalDens",mname="log(N)m^-2",title="Density")
dev.off()

# Population Area
png("4_Figures/Fig1_Pop_Size.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,mega="Pop_Size",mname="m^2",title="Area")
dev.off()

# Rosette Ratio
png("4_Figures/Fig1_RosRatio.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,mega="RosRatio",mname="Rosettes:Adults",logt=F,title="Ratio of rosettes to adults")
dev.off()



## OTHER SCHEMES

## Colour Scheme B
Clrs<-c("#B2B09B","#43AA8B","#254441")
HClr<-"#EC4E2066"
png("4_Figures/Fig1_TotalPopSizeB.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,title="Number of individuals",mega="TotalSize")
dev.off()

## Colour Scheme C
Clrs<-c("#F6B6CF","#E1B07E","#795C5F")
HClr<-"#2D085966"
png("4_Figures/Fig1_TotalPopSizeC.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,title="Number of individuals",mega="TotalSize")
dev.off()

## Colour Scheme D
Clrs<-c("#F5CB5C","#BBD8B3","#CB793A")
HClr<-"#74226C66"
png("4_Figures/Fig1_TotalPopSizeD.png",width=21,height=7,units="in",res=200)
  plotmap(pred=bioPC1,title="Number of individuals",mega="TotalSize")
dev.off()


