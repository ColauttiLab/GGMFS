############################################
## This script calculates growing degree days (GDD)
## using weather data from 7a_Import_NOAAweather.R
## then interpolates GDD for each population
############################################

##############################
## Load functions
##############################
library(fields) # Used for spatial interpolation of GDD for each population
library(zoo) # Used to impute missing data points for some weather stations in some days

##############################
## Load data
##############################
PopData<-read.csv("Population_Means_wClimate.csv")
StnData<-read.csv("WeatherRawData/NOAAStationData.csv")

##############################
## Calculate GDD for 20 closest stations for each Pop
##############################

# For each population in the dataset
# Find nearby stations in NOAAData 
# load station growing degrees for each day (GD)
# NOTE: GD = (TMAX+TMIN)/20-5 and set GD=0 if GD<0
# NOTE: /20 instead of 2 because measured in 1/10 degrees
PopData$GDD<-NA # Standard Growing-Degree Days, as above
PopData$GDays<-NA # Number of days above 5oC
Cntr<-0
# For each year: 
for(year in (2009:2013)){
  # Open file with GD data
  GDFilePath<-paste0("WeatherRawData/NOAAStnsClose",year,".csv") 
  GDData<-read.csv(GDFilePath)
  for(Pop in PopData$Pop_Code[PopData$Year==year]){ # Cycle through pop_codes sampled in same year as GD data
    Cntr<-Cntr+1
    Tmr<-Sys.time()
    # Record day of sampling
    SDay<-PopData$Day[PopData$Pop_Code==Pop]
    # Find names of nearby stations
    LocStns<-paste(unique(StnData$StationID[StnData$Pop_Code==Pop & StnData$Measure=="TMAX"]))
    # Subset GDData for stations of interest from Jan 1 to day of sampling
    PopGDData<-GDData[GDData$StationID %in% LocStns & GDData$Day<=SDay,]
    ## Remove stations with >10% missing data
    LocStns<-names(summary(PopGDData$StationID[!is.na(PopGDData$GD)])[summary(PopGDData$StationID[!is.na(PopGDData$GD)])>SDay*0.90])
    if(length(LocStns)<20){
      LocStns<-names(summary(PopGDData$StationID[!is.na(PopGDData$GD)])[summary(PopGDData$StationID[!is.na(PopGDData$GD)])>SDay*0.70])
    }
    if(length(LocStns)>20){
      # Sort remaining stations by distance
      LocStns<-unique(StnData[StnData$Pop_Code==Pop & StnData$StationID %in% LocStns,c("StationID","Dist")])
      # Keep closest 20 stations 
      LocStns<-paste(LocStns$StationID[order(LocStns$Dist)][1:20])
    }
    # Re-Subset GDData now that we have screened for missing data and distance
    PopGDData<-GDData[GDData$StationID %in% LocStns & GDData$Day<=SDay,]
    # Make data frame to collect GDD, lat and long for each station
    GeoDat<-unique(StnData[StnData$StationID %in% LocStns,1:3])
    GeoDat$GDD<-NA
    GeoDat$GDays<-NA
    # Impute missing data for each station
    for(Stn in LocStns){
      # Impute and then calculate GDD
      GeoDat$GDD[GeoDat$StationID==Stn]<-sum(na.approx(zoo(PopGDData$GD[PopGDData$StationID==Stn],0:SDay)))
      GeoDat$GDays[GeoDat$StationID==Stn]<-sum(na.approx(zoo(PopGDData$GD[PopGDData$StationID==Stn],0:SDay))>0)
      # Note: works even if no missing data
    }
    # Spatial interpolation of GDD
    if(nrow(GeoDat)>=10){ # Only interpolate if at least 10 stations available
      # Interpolate GDD for Pop
      tps<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$GDD,scale.type="unscaled") # see details in ?Tps  
      tpsD<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$GDays,scale.type="unscaled")
      PopData$GDD[PopData$Pop_Code==Pop]<-predict(tps,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")      
      PopData$GDays[PopData$Pop_Code==Pop]<-predict(tpsD,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")   
    }
    cat("***************\nIteration ",Cntr," of",length(unique(PopData$Pop_Code)),"\nYear: ",year,"\nPop: ",Pop,"\n",Sys.time()-Tmr,"seconds","\nGDD: ",PopData$GDD[PopData$Pop_Code==Pop],"\nGDays: ",PopData$GDays[PopData$Pop_Code==Pop],"\n***************")
    SDay<-LocStns<-PopGDData<-GDDs<-GeoDat<-tps<-NA # clean up for next iteration of pop
  }
  GDData<-GDFilePath<-NA # Clean-up for next iteration of year
}

# SAVE output
write.csv(PopData,"Population_Means_wClimate_wGDD.csv",row.names=F)

