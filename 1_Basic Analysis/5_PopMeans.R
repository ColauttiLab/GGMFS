##########################################################
### This script calculates population-level averages   ###
### Then it compares native vs. introduced populations ###
##########################################################
library(ggplot2)
library(grid)
library(gridExtra)

##############################
## Load FUNCTIONS
##############################
# Plotting theme
source("Functions/theme_black.R")
# Extract variables from CorData for histogram plotting and certain anayses
source("Functions/plotvars.R")
# Include bootstrap estimates of mean and P
source("Functions/regionbootstrap.R") # Default Parameters: (BData,BVar="Fruits",NIter=10000)
# Data for plotting histograms
source("Functions/plothist.R")
##############################
# Import corrected data
CorData<-read.csv("CorrectedDataAll.csv",header=T,as.is=T)
##########################################################################################

#########################
## 1. Begin with base measurements (only measured once per population)
#########################
PopMeans<-CorData[,grep("P[0-9]+",names(CorData),invert=T)] # Exclude any within-plot measurements (invert=T)
measures<-CorData[,grep('P[0-9]+',names(CorData))] # separate data.frame of plot-specific measurements
#########################

#########################
## 2. Add population averages from repeated measures
#########################
# Create a sub-data.frame with relevant data
measures<-CorData[,grep('P[0-9]+',names(CorData))]

# Calculate average number of rosette and adult plants per plot, including empty plots
PopMeans$RosCount<-apply(measures[grep('P[0-9]+Ros$',names(measures))],1,function(x) sum(x,na.rm=T)) # Count total rosettes across plots
PopMeans$AdultCount<-apply(measures[grep('P[0-9]+Adult$',names(measures))],1,function(x) sum(x,na.rm=T)) # Coutn adult plants
# NPlots unreliable; calcualte manually as the number of plots with > at least one measurement
PopMeans$NPlots<-0
for (plot in 1:10){
  PopMeans$NPlots<-PopMeans$NPlots+(rowSums(measures[grep(paste0("P",plot,"[A-z]"),names(measures))],na.rm=T)>0)
}
# Calculate average number of rosette and adult plants per plot (in plots containing at least 1 rosette or adult)
PopMeans$RosDens<-PopMeans$RosCount/PopMeans$NPlots/0.5 # divide by 0.5 to get density per m2
PopMeans$AdultDens<-PopMeans$AdultCount/PopMeans$NPlots/0.5 # divide by 0.5 to get density per m2
PopMeans$TotalDens<-(PopMeans$RosCount+PopMeans$AdultCount)/PopMeans$NPlots/0.5
# Calculate ratio of rosettes to adults (log)
PopMeans$RosRatio<-log(PopMeans$RosCount+1)-log(PopMeans$AdultCount+1) # Note: log(ros/adults) cannot deal with 100% adults or juveniles
# Estimate total number of individuals
PopMeans$TotalSize<-PopMeans$TotalDens*PopMeans$Pop_Size

# Calculate highest density of any plot in each population (Note some -Inf due to missing data in all plots; replace with NA)
PopMeans$RosDensMax<-as.numeric(gsub(-Inf,NA,apply(measures[grep('P[0-9]+Ros$',names(measures))],1,function(x) max(x,na.rm=T))))/0.5 # divide by 0.5 to get density per m2
PopMeans$AdultDensMax<-as.numeric(gsub(-Inf,NA,apply(measures[grep('P[0-9]+Adult$',names(measures))],1,function(x) max(x,na.rm=T))))/0.5 # divide by 0.5 to get density per m2
# Plot to look for errors (All points should be above 1:1 line)
qplot(x=log(RosDens+1),y=log(RosDensMax+1),data=PopMeans,geom="abline")+geom_point()#+geom_text(aes(label=Pop_Code))
qplot(x=log(AdultDens+1),y=log(AdultDensMax+1),data=PopMeans,geom="abline")+geom_point()#+geom_text(aes(label=Pop_Code))

# Check for zero values, before using in size calculations
sum(measures[grep('P[0-9]+Ros[0-9]+',names(measures))]==0,na.rm=T) # Count number of zeros for Rosette size (Should Return 0)
sum(measures[grep('P[0-9]+Adult[0-9]+',names(measures))]==0,na.rm=T) # Count number of zeros for Adult Height (Should Return 0)

# Calculate mean size
PopMeans$RosSize<-rowMeans(measures[grep('P[0-9]+Ros[0-9]+',names(measures))],na.rm=TRUE)
PopMeans$Height<-rowMeans(measures[grep('P[0-9]+Adult[0-9]+',names(measures))],na.rm=TRUE)
PopMeans$Leaves<-rowMeans(measures[grep('P[0-9]+Leaves[0-9]+',names(measures))],na.rm=TRUE)
PopMeans$Fruits<-rowMeans(measures[grep('P[0-9]+Fruits[0-9]+',names(measures))],na.rm=TRUE)
PopMeans$MaxFruits<-as.numeric(gsub(-Inf,NA,apply(measures[grep('P[0-9]+Fruits[0-9]+',names(measures))],1,function(x) max(x,na.rm=T)))) # Find max; -Inf due to no data for entire pop; replace with NA

# Calculate average damage to each plant (Pct of leaves showing damage, averaged across all individuals measured in a populations)
PopMeans$Herb<-rowMeans(measures[grep('P[0-9]+Dmg[0-9]+',names(measures))]/measures[grep('P[0-9]+Leaves[0-9]+',names(measures))],na.rm=TRUE)
PopMeans$FungDmg<-rowMeans(measures[grep('P[0-9]+Fung[0-9]+',names(measures))]/measures[grep('P[0-9]+Leaves[0-9]+',names(measures))],na.rm=TRUE)
# Calculate fungal prevalence: avg. percent of plants in each plot
PopMeans$PctRosFung<-apply(measures[grep('P[0-9]+RosFung$',names(measures))],1,function(x) sum(x,na.rm=T))/PopMeans$RosCount
PopMeans$PctAdultFung<-apply(measures[grep('P[0-9]+AdultFung$',names(measures))],1,function(x) sum(x,na.rm=T))/PopMeans$AdultCount
# Above fungus measurements replaces NAs with 0; need to fix because it wasn't measured in many cases
# First, check 0 Fung for NAs
PopMeans$PctRosFung[PopMeans$PctRosFung==0 & is.na(apply(measures[grep('P[0-9]+RosFung$',names(measures))],1,function(x) sum(x)))]<-NA
# Second, check 0 Fung for 0 rosettes - currently there are none, but it doesn't hurt to add this step
PopMeans$PctRosFung[PopMeans$PctRosFung==0 & PopMeans$RosCount==0]<-NA
# Repeat for Adults
PopMeans$PctAdultFung[PopMeans$PctAdultFung==0 & is.na(apply(measures[grep('P[0-9]+AdultFung$',names(measures))],1,function(x) sum(x)))]<-NA
PopMeans$PctAdultFung[PopMeans$PctAdultFung==0 & PopMeans$AdultCount==0]<-NA

# Calculate Total Population size (total number of individuals)
PopMeans$TotalSize<-PopMeans$TotalDens*PopMeans$Pop_Size

# Calculate Perfomance Index (based on estimated fruit production over 2-year cycle)
# See transition probabilities in Davis et al. (Ecol App 2012) --> survival over summer and winter = 0.105
PopMeans$PerfInd<-PopMeans$Fruits*(PopMeans$AdultDens*PopMeans$Pop_Size+PopMeans$RosDens*PopMeans$Pop_Size*0.105)
# log-transform
PopMeans$PerfInd<-log(PopMeans$PerfInd+1)

#########################
## 3. Plot Histograms comparing avg native vs. introduced populations
#########################
# Create directories if they don't already exist
dir.create(file.path(getwd(), "Histograms"), showWarnings = FALSE)
dir.create(file.path(getwd(), "Key Results"), showWarnings = FALSE)

### Compare Sampling Date
qplot(PopMeans$Day)
png("Histograms/SampleDate.png",width=12,height=12,units="in",res=200)
  plothist(xval="Day",BWth=20,HJust=0.2)+ggtitle("Sampling day")+xlab("Julian Day")
dev.off()
  qplot(Day,data=PopMeans[PopMeans$Latitude>47 & PopMeans$Latitude<50 &
                                       PopMeans$Longitude>7 & PopMeans$Longitude<16,])+ggtitle("Sampling date-populations near Tuebingen")+xlab("Julian Day")

### Individual Plant Performance
# Height plot
pHt<-plothist(xval="Height",BWth=15,HJust=0.2)+ggtitle("Adult height")+xlab("cm")+xlim(0,round(max(PopMeans$Height,na.rm=T),0))
# Leaves plot
pLv<-plothist(xval="Leaves",BWth=0.2)+ggtitle("Adult leaves")+xlab("N")+scale_x_log10()
# Fruits plot
pFrt<-plothist(xval="Fruits",BWth=0.2)+ggtitle("Fruits")+xlab("N")+scale_x_log10()
# Rosette size plots
pRoS<-plothist(xval="RosSize",BWth=0.2)+ggtitle("Rosette width")+xlab("cm")+scale_x_log10()
## OUTPUT
png("Histograms/Performance.png",width=21,height=14,units="in",res=200)
  grid.arrange(pHt,pLv,pFrt,pRoS,nrow=2,left=textGrob("Frequency",gp=gpar(cex=4),rot=90))
dev.off()

### Densities
# Adult Density
pAdD<-plothist(xval="AdultDens",BWth=0.4)+ggtitle("Adult density")+xlab(expression("log(N/"*m^"2"*")"))+scale_x_log10()
# Rosette Density
pRoD<-plothist(xval="RosDens",BWth=0.4)+ggtitle("Rosette density")+xlab(expression("log(N/"*m^"2"*")"))+scale_x_log10()
# Total Density
pTD<-plothist(xval="TotalDens",BWth=0.4)+ggtitle("Total density")+xlab(expression("log(N/"*m^"2"*")"))+scale_x_log10()
## OUTPUT
png("Histograms/PopDensity.png",width=21,height=14,units="in",res=200)
  grid.arrange(arrangeGrob(pAdD,pRoD,ncol=2),pTD,nrow=2,left=textGrob("Frequency",gp=gpar(cex=4),rot=90))
dev.off()

### Population performance
# Population extent (Area)
pPSi<-plothist(xval="Pop_Size",BWth=0.5)+ggtitle("Population extent")+xlab(expression("log("*m^"2"*")"))+scale_x_log10()
# Total Estimated Pop Size
pEsS<-plothist(xval="TotalSize",BWth=0.5,HLoc=1000000)+ggtitle("Total population size\n(extent * density)")+xlab("log(Individuals)")+scale_x_log10()
## OUTPUT
png("Histograms/PopSize.png",width=21,height=14,units="in",res=200)
  grid.arrange(pPSi,pEsS,nrow=2,left=textGrob("Frequency",gp=gpar(cex=4),rot=90))
dev.off()

### Rosette Ratio
pRoR<-plothist(xval="RosRatio",BWth=0.8,HLoc=6)+ggtitle("Rosette ratio")+xlab("log(Ros/Adult)")
## OUTPUT
png("Histograms/Rosette_Ratio.png",width=21,height=14,units="in",res=200)
  pRoR+ylab("Frequency")
dev.off()

### Performance Index
pPerfInd<-plothist(xval="PerfInd",BWth=2,HLoc=6)+ggtitle("Performance Index")+xlab("log(Predicted Fruits)")
png("Histograms/PerfInd.png",width=21,height=14,units="in",res=200)
  pPerfInd+ylab("Frequency")
dev.off()

### Fungal damage
# Fungal infection rates and damage
FSz<-32 # Font size
pPcRF<-plothist(xval="PctRosFung",BWth=0.01,HLoc=1,HJust=1,FntSz=FSz)+ggtitle("Rosette infect. rate")
pPcAF<-plothist(xval="PctAdultFung",BWth=0.01,HLoc=1,HJust=1,FntSz=FSz)+ggtitle("Adult infect. rate")
pFd<-plothist(xval="FungDmg",BWth=0.01,HLoc=1,HJust=1,FntSz=FSz)+ggtitle("Avg. adult dmg.")
# same but with zeros excluded
pPcRFn<-plothist(PData=PopMeans[!is.na(PopMeans$PctRosFung) & PopMeans$PctRosFung>0,],xval="PctRosFung",BWth=0.01,HLoc=1,HJust=1,FntSz=FSz)
pPcAFn<-plothist(PData=PopMeans[!is.na(PopMeans$PctAdultFung) & PopMeans$PctAdultFung>0,],xval="PctAdultFung",BWth=0.01,HLoc=1,HJust=1,FntSz=FSz)
pFdn<-plothist(PData=PopMeans[!is.na(PopMeans$FungDmg) & PopMeans$FungDmg>0,],xval="FungDmg",BWth=0.01,HLoc=1,HJust=1,FntSz=FSz)
## OUTPUT
png("Histograms/Fungal_Infection.png",width=21,height=14,units="in",res=200)
  grid.arrange(pPcRF,pPcAF,pFd,pPcRFn,pPcAFn,pFdn,ncol=3,left=textGrob("Frequency",gp=gpar(cex=3),rot=90),bottom=textGrob("Proportion",gp=gpar(cex=3))) 
dev.off()

### Herbivory Damage
pHrb<-plothist(xval="Herb",BWth=0.1,HLoc=1,HJust=1,FntSz=24)+ggtitle("Avg. herbivore damge")+xlab("Proportion of leaves")+ylab("Frequency")
## OUTPUT
png("Histograms/Herbivory.png",width=7,height=7,units="in",res=200)
  pHrb
dev.off()

###########################
### Output Population Means
###########################
write.csv(PopMeans,"Population_Means.csv", row.names=FALSE) 
