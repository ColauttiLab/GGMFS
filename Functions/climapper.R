# Makes climate graphs with/without borders and sampling locations
# Type represents the file type to use. 
require(maptools)
climapper<-function(Type="win",Borders=F,Locs=F,AllLocs=F){
  SaveName<-"BioClimMap"
  if(Locs==T){SaveName<-"BioClimMap_GGMFS"}
  if(Borders==T){SaveName<-"BioClimMap_Borders"}
  SaveName<-paste(SaveName,Type,sep=".")
  
  if(Type=="win"){
    windows(width=16.5, height=5.5, rescale="fixed") # Use this to plot in separate window  
  }
  if(Type=="svg"){
    svg(paste0("../Maps/",SaveName),width=33,height=9.2)
  }
  if(Type=="pdf"){
    svg(paste0("../Maps/",SaveName),width=33,height=9.2)
  }
  if(Type=="png"){
    png(paste0("../Maps/",SaveName),width=3300,height=920)
  }
  # Plot map with bioclim overlay
  plot(Predictors$bio36,xlim=c(-125,45),ylim=c(30,60),1,main="",xaxt="n",yaxt="n",legend=F) ## NB: Need wide, narrow plot window to get best pic
  if(Borders==T){
    data(wrld_simpl)
    plot(wrld_simpl, add=TRUE)  ## if graphing window proportions are good, then this works well, otherwise it leaves white space
  }
  if(AllLocs==T){
    AllSites<-AllRecs[,c("Longitude","Latitude")]
    points(AllSites, col="#F4007666", cex=0.1,pch=16) #simple pop locations  
  }
  if(Locs==T){
    PopLocsRnd<-PopMeans[,c("Longitude","Latitude")]+runif(382,-0.2,0.2) # add very slight random noise to better see points that are really close together
    points(PopLocsRnd, col="#003F91FF", cex=5,pch=21) #simple pop locations
    points(PopLocsRnd, col="#98B9F266", cex=3,pch=16) #simple pop locations  
  }
  
  if(Type!="win"){
    dev.off()
  }
}