###########################################################################################
### This script explores corrected measurements output from previous correction scripts ###
### see 'ggmfs-sausagemaking' repository for specific changes                           ###
###########################################################################################
library(ggplot2)
##############################
## Load FUNCTIONS
##############################
# Simplified/clean theme for plotting
source("Functions/theme_black.R")
# Function to extract repeated measures and grouping variable for plotting
source("Functions/plotvars.R")
# Function to extract repeated measures and grouping variable for plotting
source("Functions/plotcounts.R")
##############################
# Import corrected data
CorData<-read.csv("CorrectedDataAll.csv",header=T,as.is=T)
##########################################################################################

##########################################################################################
# Specify colour palette for different years
YearPlt<-c("#CCFBFE33","#E4C1F933","#FF3C3833","#ffc50b33","#DBFE8733")

## BEGIN WRITING OUTPUT TO FILE:
# Make an output directory unless it exists already
dir.create(file.path(getwd(), "DataExplore"), showWarnings = FALSE)
pdf("DataExplore/BasicGraphs.pdf")
## Basic graphs
 PData<-plotvars("Leaves","Fruits")
 ggplot(aes(x=log(Fruits+1),y=log(Leaves+1),group=Year,col=Year),data=PData) + geom_point(size=5,pch=1,alpha=0.3) + geom_point(size=5,pch=16) + geom_smooth(method=lm) + 
   ggtitle("Leaves vs Fruits") + theme_black() + scale_colour_manual(values=YearPlt)

 PData<-plotvars("Adult","Fruits")
 ggplot(aes(x=Fruits,y=Adult,group=Year,col=Year),data=PData) + geom_point(size=5,pch=1,alpha=0.5) + geom_point(size=5,pch=16) + 
   ggtitle("Adult Height vs Fruits") + theme_black() + scale_colour_manual(values=YearPlt) 

## Calculate herbivore and fungal damage proportions
 Herbiv<-c(t(CorData[,grep("P[0-9]{1,2}Dmg[0-9]*0",names(CorData))]))
 Fungal<-c(t(CorData[,grep("P[0-9]{1,2}Fung[0-9]*0",names(CorData))]))
 Leaves<-c(t(CorData[,grep("P[0-9]{1,2}Leaves[0-9]*0",names(CorData))]))
 Fruits<-c(t(CorData[,grep("P[0-9]{1,2}Fruits[0-9]*0",names(CorData))]))
 DmgData<-data.frame(HerbPct=Herbiv/Leaves,FungPct=Fungal/Leaves,Fruits=Fruits)
 Herbiv<-Fungal<-Leaves<-Fruits<-NA # Delete vectors to avoid plotting by mistake
 DmgData$Year<-factor(rep(CorData[,"Year"],each=50))
 DmgData$Pop_Code<-factor(rep(CorData[,"Pop_Code"],each=50))

 ggplot(aes(x=FungPct,y=log(Fruits+1),group=Year,col=Year),data=DmgData) + geom_point(size=5,pch=1,alpha=0.5) + geom_point(size=5,pch=16) + geom_smooth(method=lm) + 
   ggtitle("Fungal % Dmg vs Fruits") + theme_black() + scale_colour_manual(values=YearPlt)

 ggplot(aes(x=HerbPct,y=log(Fruits+1),group=Year,col=Year),data=DmgData) + geom_point(size=5,pch=1,alpha=0.5) + geom_point(size=5,pch=16) + geom_smooth(method=lm) + 
   ggtitle("Herbivory % Dmg vs Fruits") + theme_black() + scale_colour_manual(values=YearPlt)
## CLOSE FILE
dev.off()
##########################################################################################

##########################################################################################
# Specify colour palette for different years (Higher alpha because fewer overlapping points)
YearPlt<-c("#CCFBFE66","#E4C1F966","#FF3C3866","#ffc50b66","#DBFE8766")
## BEGIN WRITING OUTPUT TO FILE:
pdf("DataExplore/SeparatePops.pdf",width=36,height=36)
## Multi-faceted (by populations)
 PData<-plotvars("Leaves","Fruits")
 ModTmp<-lm(log(Leaves+1)~log(Fruits+1)*Pop_Code,data=PData)
 ggplot(aes(x=log(Fruits+1),y=log(Leaves+1),group=Year,col=Year),data=PData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + geom_smooth(method=lm) + facet_wrap("Pop_Code") +
   scale_colour_manual(values=YearPlt) +
   geom_point(data=PData[abs(ModTmp$residuals)>sd(ModTmp$residuals,na.rm=T)*3,],shape=4,colour="red") +
   ggtitle("Leaves vs Fruits") + theme_black()

 PData<-plotvars("Adult","Fruits")
 ModTmp<-lm(log(Adult+1)~log(Fruits+1)*Pop_Code,data=PData)
 ggplot(aes(x=log(Fruits+1),y=log(Adult+1),group=Year,col=Year),data=PData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + geom_smooth(method=lm) + facet_wrap("Pop_Code") + 
   scale_colour_manual(values=YearPlt) + 
   ggtitle("Adult Height vs Fruits") + theme_black()

  PData<-plotvars("Adult","Fruits")
  ggplot(aes(x=Fruits,y=Adult,group=Year,col=Year),data=PData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + facet_wrap("Pop_Code") + 
    scale_colour_manual(values=YearPlt) + 
    ggtitle("Adult Height vs Fruits") + theme_black()

  PData<-plotvars("Adult","Fruits")
  ggplot(aes(x=1/Fruits,y=1/Adult,group=Year,col=Year),data=PData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + facet_wrap("Pop_Code") + 
    scale_colour_manual(values=YearPlt) + 
    ggtitle("Adult Height vs Fruits") + theme_black()

 ModTmp<-lm(log(Fruits+1)~log(FungPct+1)*Pop_Code,data=DmgData)
 ggplot(aes(x=log(FungPct+1),y=log(Fruits+1),group=Year,col=Year),data=DmgData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + geom_smooth(method=lm) + facet_wrap("Pop_Code") + 
  scale_colour_manual(values=YearPlt) + 
  ggtitle("Fungal % Damg vs Fruits") + theme_black() 

 ModTmp<-lm(log(Fruits+1)~log(HerbPct+1)*Pop_Code,data=DmgData) 
 ggplot(aes(x=log(HerbPct+1),y=log(Fruits+1),group=Year,col=Year),data=DmgData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + geom_smooth(method=lm) + facet_wrap("Pop_Code") + 
  scale_colour_manual(values=YearPlt) + 
  ggtitle("Herb % Dmg vs Fruits") + theme_black()

 ModTmp<-lm(log(FungPct+1)~log(HerbPct+1)*Pop_Code,data=DmgData) 
 ggplot(aes(x=log(HerbPct+1),y=log(FungPct+1),group=Year,col=Year),data=DmgData) + geom_point(pch=1,alpha=0.5) + geom_point(pch=16) + geom_smooth(method=lm) + facet_wrap("Pop_Code") + 
  scale_colour_manual(values=YearPlt) + 
  ggtitle("Herb vs Fungal % Dmg") + theme_black()
## CLOSE FILE
dev.off()
##########################################################################################

##########################################################################################
## EXPLORE ADULT & ROSETTE COUNTS
pdf("DataExplore/CountData.pdf")
 PData<-plotcounts("Ros","RosFung")
 ggplot(aes(x=Ros,y=RosFung,group=Year,col=Year),data=PData) + geom_abline(intercept=0,col="grey",size=3,linetype="dashed") + geom_point(size=5) +
   ggtitle("N Ros vs N Ros with fungus") + theme_black() + scale_colour_manual(values=YearPlt)

 PData<-plotcounts("Adult","AdultFung")
 ggplot(aes(x=Adult,y=AdultFung,group=Year,col=Year),data=PData) + geom_abline(intercept=0,col="grey",size=3,linetype="dashed") + geom_point(size=5) +
   ggtitle("N Adult vs N Adults with fungus") + theme_black() + scale_colour_manual(values=YearPlt)

 PData<-plotcounts("Ros","Adult")
 ggplot(aes(x=Ros,y=Adult,group=Year,col=Year),data=PData) + geom_point(size=5) +
   ggtitle("N Rosettes vs N Adults") + theme_black() + scale_colour_manual(values=YearPlt)
dev.off()
##########################################################################################

##########################################################################################
## Compare Europe vs. NA
RegPlt<-c("#32D9EC33","#DB3A3E33") # Colours for native vs. intro

pdf("DataExplore/Region_Compare.pdf")
 PData<-plotvars("Leaves","Fruits",g="Region")
 ggplot(aes(x=log(Fruits+1),y=log(Leaves+1),group=Region,col=Region),data=PData) + geom_point(size=5) + geom_smooth(method=lm,size=2) + 
  ggtitle("Leaves vs Fruits") + theme_black() + scale_colour_manual(values=RegPlt)

 PData<-plotvars("Adult","Fruits",g="Region")
 ggplot(aes(x=log(Fruits+1),y=log(Adult+1),group=Region,col=Region),data=PData) + geom_point(size=5) + geom_smooth(method=lm,size=2) + 
  ggtitle("Adult Height vs Fruits") + theme_black() + scale_colour_manual(values=RegPlt)

 PData<-plotcounts("Ros","RosFung",g="Region")
 ggplot(aes(x=Ros,y=RosFung,group=Region,col=Region),data=PData) + geom_abline(intercept=0,col="grey",size=3,linetype="dashed") + geom_point(size=5) +
   ggtitle("N Ros vs N Ros with fungus") + theme_black() + scale_colour_manual(values=RegPlt)

 PData<-plotcounts("Adult","AdultFung",g="Region")
 ggplot(aes(x=Adult,y=AdultFung,group=Region,col=Region),data=PData) + geom_abline(intercept=0,col="grey",size=3,linetype="dashed") + geom_point(size=5) +
   ggtitle("N Adult vs N Adults with fungus") + theme_black() + scale_colour_manual(values=RegPlt)

 PData<-plotcounts("Ros","Adult",g="Region")
 ggplot(aes(x=log(Ros+1),y=log(Adult+1),group=Region,col=Region),data=PData) + geom_point(size=5) +
   ggtitle("N Rosettes vs N Adults") + theme_black() + scale_colour_manual(values=RegPlt)
dev.off()
##########################################################################################
