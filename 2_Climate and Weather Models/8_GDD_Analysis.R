############################################
## This script briefly explores Growing Degree days data

##############################
## Load functions
##############################
library(ggplot2)
library(gridExtra)

##############################
## Load data
##############################
PopData<-read.csv("Population_Means_wClimate_wGDD.csv")

##############################
## Explore effect of GDD on population means
##############################
# Check normality
qplot(GDD,data=PopData)+theme_classic()
qplot(GDays,data=PopData)+theme_classic()

# How similar are GDD and GDays (i.e. days with avg temps > Tbase? 
qplot(GDays,GDD,data=PopData)+theme_classic()

### Check GDD vs. Day of sampling and season length
png("ClimateCompare/GDD_Day_bio7.png",width=9,height=6,units="in",res=200)
  qplot(x=Day,y=GDD,size=bio7,colour=Region,data=PopData,alpha=I(0.3))+theme_classic()
dev.off()
qplot(x=Day,y=GDays,size=bio7,colour=Region,data=PopData,alpha=I(0.3))+theme_classic()

### Effects on Rosette counts, Proportion and size
RosPlt<-qplot(x=GDD,RosRatio,colour=Region,data=PopData,alpha=I(0.8))+theme_classic()+geom_smooth()
m1<-anova(lm(RosRatio~GDD*Region,PopData))
RosPlt<-RosPlt+geom_text(aes(x=100,y=7.5,label=paste0("P=",round(m1[1,"Pr(>F)"],3))),colour="black")

DensPlt<-qplot(x=GDD,log(RosDens+1),colour=Region,data=PopData,alpha=I(0.8))+theme_classic()+geom_smooth()
m2<-anova(lm(log(RosDens+1)~GDD*Region,PopData))
DensPlt<-DensPlt+geom_text(aes(x=100,y=6,label=paste0("P=",round(m2[1,"Pr(>F)"],3))),colour="black")

RatPlt<-qplot(x=GDD,log(RosSize+1),colour=Region,data=PopData,alpha=I(0.8))+theme_classic()
m3<-anova(lm(log(RosSize+1)~GDD*Region,PopData))
RatPlt<-RatPlt+geom_text(aes(x=100,y=4,label=paste0("P=",round(m3[1,"Pr(>F)"],3))),colour="black")

png("ClimateCompare/GDD_Rosettes.png",width=9,height=6,units="in",res=200)
  grid.arrange(RosPlt,DensPlt,RatPlt,nrow=3)
dev.off()

### Effect on individual plant performance?
FrtPlt<-qplot(GDD,log(Fruits+1),colour=Region,data=PopData)+theme_classic() + xlab("GDays") + ylab ("Log fruits")
m1<-anova(lm(log(Fruits+1)~GDD*Region,PopData))
FrtPlt<-FrtPlt+geom_text(aes(x=150,y=5.5,label=paste0("P=",round(m1[1,"Pr(>F)"],3))),colour="black")

HtPlt<-qplot(GDD,Height,colour=Region,data=PopData)+theme_classic() + xlab("GDays") + ylab ("Height")
m2<-anova(lm(Height~GDD*Region,PopData))
HtPlt<-HtPlt+geom_text(aes(x=150,y=125,label=paste0("P=",round(m2[1,"Pr(>F)"],3))),colour="black")

png("ClimateCompare/GDD_Adults.png",width=9,height=6,units="in",res=200)
  grid.arrange(HtPlt,FrtPlt,nrow=2)
dev.off()

# Herbivory/fungus trends?
qplot(GDD,PctAdultFung,data=PopData)+theme_classic()+geom_smooth()
qplot(GDD,PctRosFung,data=PopData)+theme_classic()+geom_smooth()
qplot(GDD,Herb,data=PopData)+theme_classic()+geom_smooth()




