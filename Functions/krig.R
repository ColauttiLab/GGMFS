# This is a custom kriging function using autoKrige in the automap library
# Var = the variable to Krige (the column name in Data)
# Region = "NorthAm" or "Europe"
# LogT = log-transform data T/F
# Area = radius size for kriging around sampling locations (Use Area=0 for convex hull)
krig<-function(Data=PopData,Var=NA,Region=NA,LogT=F,Area=10){
  require(automap)
  require(maptools)
  require(rgdal)
  # Subset data for variable of interest
  if(Region=="NorthAm"){
    Data<-Data[Data$Region=="NorthAm",]
  }
  if(Region=="Europe"){
    Data<-Data[Data$Region=="Europe",]    
  }
  KData<-Data[,c("Longitude","Latitude",Var)]
  if(LogT==T){
    KData[,Var]<-log(KData[,Var]+1)
  }
  # Remove incomplete data
  KData<-KData[complete.cases(KData),]
  # Remove repeated locations to avoid Krigin problems
  KData<-KData[!duplicated(KData[,c("Longitude","Latitude")]),]
  names(KData)<-c("long","lat",Var)
  # Convert data frame to SpatialPointsDataFrame
  coordinates(KData)<-~long+lat
  # Project onto ellipse
  proj4string(KData)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # Calculate to transformed Euclidean distances
  EucData<-spTransform(KData, CRS("+proj=merc +zone=18s +ellps=WGS84 +datum=WGS84"))
  
  ## Create custom area for interpolation (based on Area parameter) if Area >0
  if (Area>0){
    # Create a grid of points
    x<-sort(rep(-Area:Area,2*Area+Area))
    y<-rep(-Area:Area,2*Area+Area)
    # Remove points outside of radius
    Coords<-data.frame(x=x,y=y)
    Coords<-Coords[x^2+y^2<Area^2,]
    # Change scale to allow larger areas without massive resolution
    Coords<-Coords*10000
    # Apply to each data point in EucData
    EucDataFrame<-data.frame(EucData)
    InterLocs<-data.frame()
    for(i in 1:nrow(EucDataFrame)){
      TmpCoords<-cbind(EucDataFrame$long[i]+Coords$x,EucDataFrame$lat[i]+Coords$y)
      InterLocs<-rbind(InterLocs,TmpCoords)
      TmpCoords<-NA
    }
    # Round values to create evenly spaced grid
    InterLocs<-round(InterLocs/50000,0)*50000
    # Remove duplicates
    InterLocs<-InterLocs[!duplicated(InterLocs),]
    #qplot(x=V1,y=V2,data=InterLocs,size=I(1),shape=I(20))+theme_classic()+xlim(-9000000,-8750000)+ylim(5250000,5500000)
    names(InterLocs)<-c("long","lat")
    coordinates(InterLocs)<-~long+lat
    proj4string(InterLocs)<-"+proj=merc +zone=18s +ellps=WGS84 +datum=WGS84" # Define current projection
    
    # Kriging to interpolate; custom area
    Krig<-autoKrige(as.formula(paste(Var,"~ 1")),input_data=EucData,verbose=T,new_data=InterLocs)    
  } else {
    # Kriging to interpolate; default area
    Krig<-autoKrige(as.formula(paste(Var,"~ 1")),input_data=EucData,verbose=T)    
  }
  return(Krig)
}