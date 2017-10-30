############################################
## This script is for plotting semivariograms
# Create directories if they don't already exist
dir.create(file.path(getwd(), "ModelSelection"), showWarnings = FALSE)

##############################
## Load functions
##############################
library(gstat)
library(ape)
library(ggplot2)
library(gridExtra)
source("Functions/geodist.R")
source("Functions/theme_black.R")
source("Functions/krig.R")

##############################
## Load data
##############################
PopData<-read.csv("Population_Means_wClimate_wGDD.csv")


##############################
## Check for spatial autocorrelation
##############################
## Calculating Moran's I
# Calculate pairwise Geodetic distance between each population
PopDist<-matrix(nrow=nrow(PopData),ncol=nrow(PopData))
for(i in 1:nrow(PopData)){
  for (j in i:nrow(PopData)){
    PopDist[i,j]<-PopDist[j,i]<-geodist(round(c(PopData$Latitude[i],PopData$Longitude[i],PopData$Latitude[j],PopData$Longitude[j]),4))
  }
}
# Take inverse (set diagonal to 0)
PopDistInv<-1/(1+PopDist)
diag(PopDistInv)<-0
# Calulate significance of Moran's I
Traits<-c("Height","Fruits","StdzHeight","StdzFruits","TotalDens","EstSize","TotalSize","RosRatio","Herb","FungDmg","PctRosFung","PctAdultFung")
I<-apply(PopData[,Traits],2,function(x) Moran.I(x,PopDistInv,na.rm=T)$p.value)
I<-data.frame(Traits=Traits,P=as.vector(I))
I$Sig<-0
I$Sig[I$P<0.05]<-1
I



########## EXPERIMENT##########
## EXTRACT VARIOGRAM FROM CUSTOM krig FUNCTION?
## Visualize variograms
# Use custom function krig.R and extract semi-variogram from output
Vgram<-function(Data=PopData,Var=NA,Region=NA,LogT=F,Clr="#ECDD7B"){
  KDat<-krig(Data=Data,Var=Var,Region=Region,LogT=LogT)
  VDat<-KDat$exp_var
  
  autofitVariogram(VDat) 
  
  n<-KDat$var_model[1,2]
  s<-KDat$var_model[2,2]
  r<-KDat$var_model[2,3]
  h<-VDat$dist
  v<-(s-n)*(1-exp(-h^2/(r^2)))
  Vgm<-vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)
  plot(VDat,model=tmp)

  
  K/(1+exp(No+r*Day))
  tmp<-
    
  plot(VDat,model=tmp)
  
  ggplot(aes(x=dist,y=gamma),data=VDat)+geom_point(color=Clr,size=4)+
            geom_smooth(method=vgm)      
    
    geom_text(aes(label=np),vjust=-1,color="white")+xlab("Distance")+ylab("Semi-variance")+theme_black()
}
########## END EXPERIMENT ##########



# Function to calculate and graph varigram
Vgram<-function(Var=NA,InData=PopData,MaxDist=10,graph=T,Type="smooth",Clr="#ECDD7B",Sub="none"){
  Data<-InData[!is.na(PopData[,Var]),] # remove missing data
  # Run subset for native or introduced range?
  if(Sub=="NorthAm"){
    Data<-Data[Data$Region=="NorthAm",]
  }
  if(Sub=="Europe"){
    Data<-Data[Data$Region=="Europe",]
  }
  # Calculate semivariance
  Vgr<-variog(coords=Data[,c("Latitude","Longitude")],data=Data[,Var],option=Type)
  if(graph==F){
    return(data.frame(Dist=Vgr$u,Var=Var,SemiVar=Vgr$v,N=Vgr$n.data))
  }
  if(graph==T){
    VgramDat<-data.frame(Dist=Vgr$u,Var=Var,SemiVar=Vgr$v,N=Vgr$n.data)
    if(MaxDist>0){
      VgramDat<-VgramDat[VgramDat$Dist<MaxDist,]
    }
    P<-qplot(x=Dist,y=SemiVar,data=VgramDat,alpha=I(ifelse(Type!="bin",0.3,1)),colour=I(Clr))+
      geom_text(aes(x=0,y=max(VgramDat$SemiVar,na.rm=T)),label=Var,colour="white",hjust=-0.2)+labs(x="",y="")+theme_black()
    return(P)
  }
}
# Create plots
Plots<-list()
for(i in 1:length(Traits)){
  Plots[[i]]<-Vgram(Trait=Traits[i],MaxDist=0)
}
png("ModelSelection/VariogramsAll.png",width=21,height=14,units="in",res=200,pointsize=0)
  do.call(grid.arrange,Plots)
dev.off()
png("ModelSelection/VariogramsAdults.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[1:4],left="Semivariance",sub="Distance"))
dev.off()
png("ModelSelection/VariogramsPops.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[5:8],left="Semivariance",sub="Distance"))
dev.off()
png("ModelSelection/VariogramsBiotic.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[9:12],left="Semivariance",sub="Distance"))
dev.off()

# Create plots for NATIVE POPULATIONS ONLY
Plots<-list()
for(i in 1:length(Traits)){
  Plots[[i]]<-Vgram(Traits[i],Type="smooth",Clr="#4FB0C6",Sub="Europe")
}
png("ModelSelection/VariogramsAll_EU.png",width=21,height=14,units="in",res=200,pointsize=0)
  do.call(grid.arrange,Plots)
dev.off()
png("ModelSelection/VariogramsAdults_EU.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[1:4],left="Semivariance",sub="Distance"))
dev.off()
png("ModelSelection/VariogramsPops_EU.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[5:8],left="Semivariance",sub="Distance"))
dev.off()
png("ModelSelection/VariogramsBiotic_EU.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[9:12],left="Semivariance",sub="Distance"))
dev.off()

# Create plots for INTRODUCED POPULATIONS ONLY
Plots<-list()
for(i in 1:length(Traits)){
  Plots[[i]]<-Vgram(Traits[i],Type="smooth",Clr="#F53751",Sub="NorthAm")
}
png("ModelSelection/VariogramsAll_NA.png",width=21,height=14,units="in",res=200,pointsize=0)
  do.call(grid.arrange,Plots)
dev.off()
png("ModelSelection/VariogramsAdults_NA.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[1:4],left="Semivariance",sub="Distance"))
dev.off()
png("ModelSelection/VariogramsPops_NA.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[5:8],left="Semivariance",sub="Distance"))
dev.off()
png("ModelSelection/VariogramsBiotic_NA.png",width=14,height=7,units="in",res=200,pointsize=24)
  do.call(grid.arrange,c(Plots[9:12],left="Semivariance",sub="Distance"))
dev.off()

# Optional: Different format
PlotClds<-list()
for(i in 1:length(Traits)){
  PlotClds[[i]]<-Vgram(Traits[i],Type="cloud")+geom_smooth(method="gam")
}
do.call(grid.arrange,PlotClds)

