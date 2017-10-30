################################################################
### This script compares Native and Introduced populations,  ###
### WHILE ACCOUNTING FOR SPATIALLY AUTOCORRELATED ERRORS     ###
### NOTE: no output files; results in command-line only      ###
################################################################

library(ggplot2)
library(nlme)
library(glmulti)
# Load custom funciton
source("functions/regionbootstrap.R")

##############################
## Load data
##############################
PopMeans<-read.csv("Population_Means.csv")
SubData<-PopMeans[!duplicated(PopMeans[,c("Longitude","Latitude")]),]

### Basic GLS models with spatially autocorrelated erros
# Individual plant Height (population average)
Height<-gls(Height~1+Region,data=SubData,correlation=corExp(form=~Longitude+Latitude|Region),na.action=na.exclude) # Add spatial structure
anova(Height)
summary(Height)
summary(lm(Height~1+Region,data=SubData)) # Compare to simple linear model

# Total population size (Extent x Density)
TotalSize<-gls(log(TotalSize+1)~1+Region,data=SubData,correlation=corExp(form=~Longitude+Latitude|Region),na.action=na.exclude) # Add spatial structure
anova(TotalSize)
summary(TotalSize)
summary(lm(log(TotalSize+1)~1+Region,data=SubData,na.action=na.exclude)) # Add spatial structure
# Estimate mean difference between regions
Fits<-exp(TotalSize$fitted)-1
Reg<-TotalSize$groups
mean(Fits[Reg=="NorthAm"])-mean(Fits[Reg=="Europe"])

# Rosette ratio
RosRatio<-gls(RosRatio~1+Region,data=SubData,correlation=corExp(form=~Longitude+Latitude|Region),na.action=na.exclude) # Add spatial structure
anova(RosRatio)
summary(RosRatio)
summary(lm(RosRatio~1+Region,data=SubData,na.action=na.exclude))
exp(summary(RosRatio)$coefficients[2])








