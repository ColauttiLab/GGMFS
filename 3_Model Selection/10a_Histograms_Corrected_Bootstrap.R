#########################
## This script tests differences between ranges
## Correcting for spatially autocorrelated error
#########################

Nreps<-10 # Number of bootstrapped replicates

#########################
## Libraries            #
#########################
library(ggplot2)
library(nlme)

#########################
## Data                 #
#########################
PCData<-read.csv("PCData.csv")
# Remove missing data
MData<-PCData[,c("Region","Fruits","TotalDens","RosRatio","Pop_Size","Latitude","Longitude")]
MData<-MData[complete.cases(MData),]
# Can't have zero spatial distance; Add slight lat/long deviations to replicated sites
MData<-MData[!duplicated(MData[,c("Latitude","Longitude")]),]
# OR eliminate dupli
#MData<-MData[!duplicated(MData[,c("Latitude","Longitude")]),]

# Calculate Region Effect after correcting for spatial autocorrelation
X<-c(1:(Nreps+1))*NA
Coefs<-data.frame(Fruits=X,TotalDens=X,RosRatio=X,Pop_Size=X)

Coefs$Fruits<-coef(gls(log(Fruits+1)~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
Coefs$TotalDens<-coef(gls(log(TotalDens+1)~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
Coefs$RosRatio<-coef(gls(RosRatio~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
Coefs$Pop_Size<-coef(gls(log(Pop_Size+1)~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]

########## CONTINUE HERE
# Set up data for permutation test

ptm <- proc.time()
for(rep in 1:Nreps){
  RepData<-MData[sample(1:nrow(RepData),nrow(RepData),replace=T),]
  Coefs$Fruits[rep+1]<-coef(gls(log(Fruits+1)~1+Region,data=RepData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
  Coefs$TotalDens[rep+1]<-coef(gls(log(TotalDens+1)~1+Region,data=RepData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
  Coefs$RosRatio[rep+1]<-coef(gls(RosRatio~1+Region,data=RepData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
  Coefs$Pop_Size[rep+1]<-coef(gls(log(Pop_Size+1)~1+Region,data=RepData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude))[2]
  RepData<-NA
}
proc.time() - ptm



# Calculate Means
Means<-colMeans(MData[MData$Region=="Europe",c("Fruits","TotalDens","RosRatio","Pop_Size")],na.rm=T)

## Translate estimated effect size to original scale of measurement
Diffs<-exp(Coefs)*Means-Means 
Diffs$RosRatio<-exp(Coefs$RosRatio) # RosRatio already a ratio



Coefs$Fruits<-coef(lm(log(Fruits+1)~1+Region,data=MData,na.action=na.exclude))[2]
Coefs$TotalDens<-coef(lm(log(TotalDens+1)~1+Region,data=MData,na.action=na.exclude))[2]
Coefs$RosRatio<-coef(lm(RosRatio~1+Region,data=MData,na.action=na.exclude))[2]
Coefs$Pop_Size<-coef(lm(log(Pop_Size+1)~1+Region,data=MData,na.action=na.exclude))[2]






# Exclude missing data
MData<-PCData[complete.cases(PCData[,c("Region","PCFruits","PCTotalDens","PCRosRatio","PCPopSize","Altitude","GDD","Understory","CoverPic_Mean","Pct_Canopy_Cover","Herb","FungDmg","PctRosFung","PctAdultFung")]),]
# Can't have zero spatial distance; Add slight lat/long deviations to replicated sites
MData<-MData[!duplicated(MData[,c("Latitude","Longitude")]),]

## Load model fits
R<-read.csv("Model_Performance.csv",row.names=1)





#########################
## Venn Diagram         #
#########################
## PROBLEM: Impossible to scale circles given these values
dmy<-data.frame(x=c(-500,500),y=c(-500,500))
## Parameters
Clrs<-c("#FBACBE","#77B6EA","#B9D2B1") # Colours for top, left & right circles
Labs<-c("Range","Abiotic","Biotic")
Ofst<-5 # Spacing of circles
Size<-20 # Circle Diameter
LSz<-12 # Label sizes

# Function to draw circles
circleFun <- function(npoints=Size*100){
  tt<-seq(pi,3*pi,length.out=npoints)
  data.frame(x=Size*cos(tt),y=Size*sin(tt))
}
crcl <- circleFun()

# Function to draw Venn diagram
venndraw<-function(){
  P<-ggplot(data=dmy,aes(x=0,y=0))+
    ## Add Circles
    geom_polygon(aes(x,y+sqrt(Ofst^2/2)),data=crcl,fill=Clrs[1],alpha=0.3)+
    geom_polygon(aes(x-Ofst,y-Ofst),data=crcl,fill=Clrs[2],alpha=0.3)+
    geom_polygon(aes(x+Ofst,y-Ofst),data=crcl,fill=Clrs[3],alpha=0.3)+
    ## Add circle labels
    geom_text(aes(x=0,y=Size+sqrt(Ofst^2/2),label=Labs[1]),colour=Clrs[1],size=LSz,vjust=-0.5)+ ## Top Text
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=-sqrt(Size^2/2)-Ofst,label=Labs[2]),colour=Clrs[2],size=LSz,hjust=1,vjust=1)+ ## Bottom-left text
    geom_text(aes(x=sqrt(Size^2/2)+Ofst,y=-sqrt(Size^2/2)-Ofst,label=Labs[3]),colour=Clrs[3],size=LSz,hjust=0,vjust=1)+ ## Bottom-left text
    ## Add main values
    geom_text(aes(x=0,y=Size+sqrt(Ofst^2/2),label=Vals[1]),size=FSz[1],vjust=1.5)+
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=-sqrt(Size^2/2)-Ofst,label=Vals[2]),size=FSz[2],vjust=-1,hjust=1)+
    geom_text(aes(x=sqrt(Size^2/2)+Ofst,y=-sqrt(Size^2/2)-Ofst,label=Vals[3]),size=FSz[3],vjust=-1,hjust=0)+
    ## Add pairwise values
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=sqrt(Size^2/2)-Ofst,label=Vals[4]),size=FSz[4],vjust=1,hjust=0)+ # 12
    geom_text(aes(x=+sqrt(Size^2/2)+Ofst,y=sqrt(Size^2/2)-Ofst,label=Vals[5]),size=FSz[5],vjust=1,hjust=1)+ #13
    geom_text(aes(x=0,y=-Size-sqrt(Ofst^2/2),label=Vals[6]),size=FSz[6],vjust=0,hjust=0.5)+ # 23
    ## Add 3-way
    geom_text(aes(x=0,y=0,label=Vals[7]),size=FSz[7])+ #123
    xlim(-1.5*Size-sqrt(Ofst^2/2),1.5*Size+sqrt(Ofst^2/2))+ylim(-1.5*Size-sqrt(Ofst^2/2),1.5*Size+sqrt(Ofst^2/2))+theme_map()
  return(P)
}

RsqVals<-data.frame()

InData<-c(VFruitsReg,VFruitsAbio,VFruitsBio,VFruitsRegAbio,VFruitsRegBio,VFruitsAbioBio,VFruitsFull)
FSz<-4+6*InData/max(InData) # Vector of scaled font sizes
Vals<-formatC(InData,digits=3,format="f") # Set decimal places to show
venndraw()+ggtitle("Fruits")

InData<-c(VDensReg,VDensAbio,VDensBio,VDensRegAbio,VDensRegBio,VDensAbioBio,VDensFull)
FSz<-4+6*InData/max(InData) # Vector of scaled font sizes
Vals<-formatC(InData,digits=3,format="f") # Set decimal places to show
venndraw()+ggtitle("Density")

InData<-c(VRosReg,VRosAbio,VRosBio,VRosRegAbio,VRosRegBio,VRosAbioBio,VRosFull)
FSz<-4+6*InData/max(InData) # Vector of scaled font sizes
Vals<-formatC(InData,digits=3,format="f") # Set decimal places to show
venndraw()+ggtitle("Rosettes/Adults")

InData<-c(VPopSizeReg,VPopSizeAbio,VPopSizeBio,VPopSizeRegAbio,VPopSizeRegBio,VPopSizeAbioBio,VPopSizeFull)
FSz<-4+6*InData/max(InData) # Vector of scaled font sizes
Vals<-formatC(InData,digits=3,format="f") # Set decimal places to show
venndraw()+ggtitle("Area")