#########################
## This script plots Histograms with Venn Diagrams
#########################

#########################
## Libraries            #
#########################
library(ggplot2)
library(nlme)
library(glmulti)
#library(MuMIn)
source("Functions/theme_map.R")

#########################
## Data                 #
#########################
PCData<-read.csv("PCData.csv")
PCData$Managed<-1-apply(PCData[,c("Hand_Removal","Herbicide","Mowing","Biocontrol")],1,function(x) sum(x,na.rm=T)<1)
# Can't have zero spatial distance; Add slight lat/long deviations to replicated sites
MData<-PCData
MData$Latitude<-MData$Latitude+rnorm(nrow(MData),mean=0,sd=0.0001)
MData$Longitude<-MData$Latitude+rnorm(nrow(MData),mean=0,sd=0.0001)

## Automated Model selection
# Identify variables with missing data
DataPoints<-apply(PCData,2,function(x) sum(!is.na(x)))
DataPoints[DataPoints<404]
# Fungi data very low (not surprising since it wasn't sampled in first few years)
# Do separate models

####################################
###  SET-UP MODEL SELECTION      ###
####################################
RespFruits<-"log(Fruits+1)"
RespDens<-"log(TotalDens+1)"
RespRos<-"RosRatio"
RespPopSize<-"log(Pop_Size+1)"

# Custom fuction for gls with spatially autocorrelated error term
# SEE: https://vcalcagnoresearch.wordpress.com/package-glmulti/
mygls<-function(y,data,na.action){
  return(gls(model=y,data=data,
             correlation=corExp(form=~Longitude+Latitude|Region),na.action=na.action))
}  
# Function for GLM multi given predictor and response variables
ModSel<-function(RespIn=NA,PredIn=NA,Data=MData,Ex=NA){
  return(glmulti(RespIn,PredIn,data=Data,level=1,fitfunction=mygls,method="h",plotty=T,na.action=na.exclude))
}

####################################
###  Model Selection             ###
####################################
#########################
## Range                #
#########################
## Basic Range Model
RegFruits<-gls(log(Fruits+1)~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)
RegDens<-gls(log(TotalDens+1)~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)
RegRos<-gls(RosRatio~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)
RegPopSize<-gls(log(Pop_Size+1)~1+Region,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)

## Calculate R2 for Venn diagram
VFruitsReg<-cor(RegFruits$fitted,RegFruits$fitted+RegFruits$residuals)^2
VDensReg<-cor(RegDens$fitted,RegDens$fitted+RegDens$residuals)^2
VRosReg<-cor(RegRos$fitted,RegRos$fitted+RegRos$residuals)^2
VPopSizeReg<-cor(RegPopSize$fitted,RegPopSize$fitted+RegPopSize$residuals)^2

#########################
## Abiotic              #
#########################
## Base climate model with spatially autocorrelated error
AbioFruits<-gls(log(Fruits+1)~1+PCFruits,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)
AbioDens<-gls(log(TotalDens+1)~1+PCTotalDens,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)
AbioRos<-gls(RosRatio~1+PCRosRatio,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)
AbioPopSize<-gls(log(Pop_Size+1)~1+PCPopSize,data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude)

## Abiotic predictors
Pred<-c("Altitude","GDD","Understory","CoverPic_Mean","Pct_Canopy_Cover")

## Model selection
AbioFruitsSel<-ModSel(RespIn=RespFruits,PredIn=c("PCFruits",Pred),Data=MData)
AbioDensSel<-ModSel(RespIn=RespDens,PredIn=c("PCTotalDens",Pred),Data=MData)
AbioRosSel<-ModSel(RespIn=RespRos,PredIn=c("PCRosRatio",Pred),Data=MData)
AbioPopSizeSel<-ModSel(RespIn=RespPopSize,PredIn=c("PCPopSize",Pred),Data=MData)

## Find 'best' gls model
AbioFruits<-gls(formula(summary(AbioFruitsSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
AbioDens<-gls(formula(summary(AbioDensSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
AbioRos<-gls(formula(summary(AbioRosSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
AbioPopSize<-gls(formula(summary(AbioPopSizeSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")

## Calculate R2 for Venn diagram
VFruitsAbio<-cor(AbioFruits$fitted,AbioFruits$fitted+AbioFruits$residuals)^2
VDensAbio<-cor(AbioDens$fitted,AbioDens$fitted+AbioDens$residuals)^2
VRosAbio<-cor(AbioRos$fitted,AbioRos$fitted+AbioRos$residuals)^2
VPopSizeAbio<-cor(AbioPopSize$fitted,AbioPopSize$fitted+AbioPopSize$residuals)^2

#########################
## Biotic Effects       #
#########################
## Abiotic predictors
Pred<-c("Herb","FungDmg","PctRosFung","PctAdultFung")

## Model selection
BioFruitsSel<-ModSel(RespIn=RespFruits,PredIn=c(Pred),Data=MData)
BioDensSel<-ModSel(RespIn=RespDens,PredIn=c(Pred),Data=MData)
BioRosSel<-ModSel(RespIn=RespRos,PredIn=c(Pred),Data=MData)
BioPopSizeSel<-ModSel(RespIn=RespPopSize,PredIn=c(Pred),Data=MData)

## Find 'best' gls model
BioFruits<-gls(formula(summary(BioFruitsSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
BioDens<-gls(formula(summary(BioDensSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
BioRos<-gls(formula(summary(BioRosSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
BioPopSize<-gls(formula(summary(BioPopSizeSel)$bestmodel),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")

## Calculate R2 for Venn diagram
VFruitsBio<-cor(BioFruits$fitted,BioFruits$fitted+BioFruits$residuals)^2
VDensBio<-cor(BioDens$fitted,BioDens$fitted+BioDens$residuals)^2
VRosBio<-cor(BioRos$fitted,BioRos$fitted+BioRos$residuals)^2
VPopSizeBio<-cor(BioPopSize$fitted,BioPopSize$fitted+BioPopSize$residuals)^2

#########################
## Range+Abiotic        #
#########################
RegAbioFruits<-gls(formula(paste(summary(AbioFruitsSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
RegAbioDens<-gls(formula(paste(summary(AbioDensSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
RegAbioRos<-gls(formula(paste(summary(AbioRosSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
RegAbioPopSize<-gls(formula(paste(summary(AbioPopSizeSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")

## Calculate R2 for Venn diagram
VFruitsRegAbio<-cor(RegAbioFruits$fitted,RegAbioFruits$fitted+RegAbioFruits$residuals)^2
VDensRegAbio<-cor(RegAbioDens$fitted,RegAbioDens$fitted+RegAbioDens$residuals)^2
VRosRegAbio<-cor(RegAbioRos$fitted,RegAbioRos$fitted+RegAbioRos$residuals)^2
VPopSizeRegAbio<-cor(RegAbioPopSize$fitted,RegAbioPopSize$fitted+RegAbioPopSize$residuals)^2

#########################
## Range+Biotic         #
#########################
RegBioFruits<-gls(formula(paste(summary(BioFruitsSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
RegBioDens<-gls(formula(paste(summary(BioDensSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
RegBioRos<-gls(formula(paste(summary(BioRosSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
RegBioPopSize<-gls(formula(paste(summary(BioPopSizeSel)$bestmodel,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")

## Calculate R2 for Venn diagram
VFruitsRegBio<-cor(RegBioFruits$fitted,RegBioFruits$fitted+RegBioFruits$residuals)^2
VDensRegBio<-cor(RegBioDens$fitted,RegBioDens$fitted+RegBioDens$residuals)^2
VRosRegBio<-cor(RegBioRos$fitted,RegBioRos$fitted+RegBioRos$residuals)^2
VPopSizeRegBio<-cor(RegBioPopSize$fitted,RegBioPopSize$fitted+RegBioPopSize$residuals)^2

#########################
## Biotic+Abiotic       #
#########################
FruitsMod<-paste(summary(AbioFruitsSel)$bestmodel,gsub("log\\(Fruits \\+ 1\\) ~ 1 ","",summary(BioFruitsSel)$bestmodel))
DensMod<-paste(summary(AbioDensSel)$bestmodel,gsub("log\\(TotalDens \\+ 1\\) ~ 1 ","",summary(BioDensSel)$bestmodel))
RosMod<-paste(summary(AbioRosSel)$bestmodel,gsub("RosRatio ~ 1 ","",summary(BioRosSel)$bestmodel))
PopSizeMod<-paste(summary(AbioPopSizeSel)$bestmodel,gsub("log\\(Pop_Size \\+ 1\\) ~ 1 ","",summary(BioPopSizeSel)$bestmodel))

AbioBioFruits<-gls(formula(FruitsMod),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
AbioBioDens<-gls(formula(DensMod),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
AbioBioRos<-gls(formula(RosMod),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
AbioBioPopSize<-gls(formula(PopSizeMod),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")

## Calculate R2 for Venn diagram
VFruitsAbioBio<-cor(AbioBioFruits$fitted,AbioBioFruits$fitted+AbioBioFruits$residuals)^2
VDensAbioBio<-cor(AbioBioDens$fitted,AbioBioDens$fitted+AbioBioDens$residuals)^2
VRosAbioBio<-cor(AbioBioRos$fitted,AbioBioRos$fitted+AbioBioRos$residuals)^2
VPopSizeAbioBio<-cor(AbioBioPopSize$fitted,AbioBioPopSize$fitted+AbioBioPopSize$residuals)^2

#########################
## Biotic+Abiotic+Region#
#########################
FullFruits<-gls(formula(paste(FruitsMod,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
FullDens<-gls(formula(paste(DensMod,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
FullRos<-gls(formula(paste(RosMod,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")
FullPopSize<-gls(formula(paste(PopSizeMod,"+ Region")),data=MData,correlation=corExp(form=~Longitude+Latitude|Region,nugget=T),na.action=na.exclude,method="ML")

## Calculate R2 for Venn diagram
VFruitsFull<-cor(FullFruits$fitted,FullFruits$fitted+FullFruits$residuals)^2
VDensFull<-cor(FullDens$fitted,FullDens$fitted+FullDens$residuals)^2
VRosFull<-cor(FullRos$fitted,FullRos$fitted+FullRos$residuals)^2
VPopSizeFull<-cor(FullPopSize$fitted,FullPopSize$fitted+FullPopSize$residuals)^2

######################################
#####################################CONTINUE HERE

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
venndraw<-function(InData){
  InData<-signif(InData,digits=3)
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
    geom_text(aes(x=0,y=Size+sqrt(Ofst^2/2),label=InData[1]),size=FSz[1],vjust=1.5)+
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=-sqrt(Size^2/2)-Ofst,label=InData[2]),size=FSz[2],vjust=-1,hjust=1)+
    geom_text(aes(x=sqrt(Size^2/2)+Ofst,y=-sqrt(Size^2/2)-Ofst,label=InData[3]),size=FSz[3],vjust=-1,hjust=0)+
    ## Add pairwise values
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=sqrt(Size^2/2)-Ofst,label=InData[4]),size=FSz[4],vjust=1,hjust=0)+ # 12
    geom_text(aes(x=0,y=-Size-sqrt(Ofst^2/2),label=InData[5]),size=FSz[5],vjust=0,hjust=0.5)+ # 23
    geom_text(aes(x=+sqrt(Size^2/2)+Ofst,y=sqrt(Size^2/2)-Ofst,label=InData[6]),size=FSz[6],vjust=1,hjust=1)+ #13
    ## Add 3-way
    geom_text(aes(x=0,y=0,label=InData[7]),size=FSz[7])+ #123
    xlim(-1.5*Size-sqrt(Ofst^2/2),1.5*Size+sqrt(Ofst^2/2))+ylim(-1.5*Size-sqrt(Ofst^2/2),1.5*Size+sqrt(Ofst^2/2))+theme_map()
  return(P)
}

#  InData<-round(InData,3)
FSz<-6+4*InData/max(InData) # Vector of scaled font sizes

venndraw(c(VFruitsReg,VFruitsAbio,VFruitsBio,VFruitsRegAbio,VFruitsRegBio,VFruitsAbioBio,VFruitsFull))

## Venn Diagram
venndraw<-function(reg,abio,bio,reg_abio,reg_bio,abio_bio,full){
  InDat<-c(reg+reg_abio+reg_bio+full,abio+reg_abio+abio_bio+full,bio+reg_bio+abio_bio+full,reg_abio+full,reg_bio+full,abio_bio+full,full)
  InDat<-signif(InDat,3)
  ggplot(data=dmy,aes(x=0,y=0))+

}


### OLD
## Another option
library(venneuler)
venndraw<-function(reg,abio,bio,reg_abio,reg_bio,abio_bio,full){
  InDat<-c(reg+reg_abio+reg_bio+full,abio+reg_abio+abio_bio+full,bio+reg_bio+abio_bio+full,reg_abio+full,reg_bio+full,abio_bio+full,full)
  InDat<-signif(InDat,6)
  names(InDat)<-c("Region","Abiotic","Biotic","Region&Abiotic","Region&Biotic","Abiotic&Biotic","Region&Abiotic&Biotic")
  
  grid.newpage()
  draw.triple.venn (area1=InDat[1],area2=InDat[2],area3=InDat[3],
                      n12=InDat[4],n13=InDat[5],n23=InDat[6],n123=InDat[7],
                      category=c("Range","Abiotic","Biotic"),
                      fill=c("#56CBF9","#D6F599","#F2CEE6"),rotation.degree=-60)
  
  #                 rotation=2,reverse=F,euler.d=T,scaled=T)
}

venndraw(reg=VFruitsReg,abio=VFruitsAbio,bio=VFruitsBio,reg_abio=VFruitsRegAbio,reg_bio=VFruitsRegBio,abio_bio=VFruitsAbioBio,full=VFruitsFull)
venndraw(reg=VDensReg,abio=VDensAbio,bio=VDensBio,reg_abio=VDensRegAbio,reg_bio=VDensRegBio,abio_bio=VDensAbioBio,full=VDensFull)
venndraw(reg=VRosReg,abio=VRosAbio,bio=VRosBio,reg_abio=VRosRegAbio,reg_bio=VRosRegBio,abio_bio=VRosAbioBio,full=VRosFull)
venndraw(reg=VPopSizeReg,abio=VPopSizeAbio,bio=VPopSizeBio,reg_abio=VPopSizeRegAbio,reg_bio=VPopSizeRegBio,abio_bio=VPopSizeAbioBio,full=VPopSizeFull)


#########################
## Chord Diagram        #
#########################
#http://cran.r-project.org/web/packages/circlize/vignettes/circular_visualization_of_matrix.pdf
#rows: Region, Biotic, Abiotic
#cols: Full Model

library(circlize)
function(reg,abio,bio,reg_abio,reg_bio,abio_bio,full){
  InMat<-data.frame(Region=c(reg,reg_abio,reg_bio),Abiotic=c(reg_abio,abio,abio_bio),Biotic=c(reg_bio,abio_bio,bio))
  rownames(InMat)<-c("Region","Abiotic","Biotic")
  InMat<-as.matrix(InMat*1000)
  chordDiagram(InMat)
  circos.clear()
  
  InMat[2,1]
  
}


