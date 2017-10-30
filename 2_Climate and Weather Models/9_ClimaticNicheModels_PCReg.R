############################################
## This script models climate effects
## using principal components regression

##############################
## Load functions
##############################
library(ggplot2)
library(gridExtra)
#library(nlme)
library(vegan)
#source("functions/modelsel.R")

##############################
## Load data
##############################
PopData<-read.csv("Population_Means_wClimate_wGDD.csv")
PopData$Region<-factor(PopData$Region,levels=c("NorthAm","Europe"))

##############################
## Niche Model (with spatially autocorrelated error)
##############################
## Theory: 
# A redundancy analysis (RDA) with a single response vector (rather than matrix)
# simplifies to a multiple regression
# But to prevent overfitting: principal components regression:
# 1. Standard PCA
# 2. Remove PCs with minimal eigenvalues
# 3. Regression on PC axes
# 4. Back-transformation of loadings from regression and PCA to identify key bioclim variables
# See Legendre & Legendre (2012) pg. 562-563

##############################
## 1.  PCA of bioclim variables
##############################
# Extract pcs from bio1-bio35 except 3 and 7 (which are calculated from other bios; bio 36:40 are PCs of the global data)
PCs<-PopData[,grep("bio",names(PopData))[1:35][c(-3,-7)]]
### NOTE: PCs 6, 22 and 30 are reversed to improve interpretation (e.g. bio5 is max temp; bio6 is min temp)
PCs$bio6<--PCs$bio6
PCs$bio22<--PCs$bio22
PCs$bio30<--PCs$bio30

# Replace original bioclim variables
PopData<-cbind(PopData[,grep("bio.*",names(PopData),invert=T)],PCs)

## Standardize bio variables to mean=0, sd=1
## Option not used (PCRegression uses correlation coefficients)
#for (col in names(PCs)[grep("bio",names(PCs))]){
#  PCs[,col]<-(PCs[,col]-mean(PCs[,col],na.rm=T))/sd(PCs[,col],na.rm=T)
#}

# Run principal components
bioPCs<-princomp(PCs,cor=T)
summary(bioPCs)
PCA<-1 # Choose PCs for plotting
PCB<-2
biplot(bioPCs,choices=c(PCA,PCB),xlabs=rep("",nrow(bioPCs$scores))) # Plot loadings of PCs 1&2
# Scale plot for adding points (note: approximation only!)
rrr<-apply(bioPCs$scores[,c(PCA,PCB)],2, range) 
rrr<-(abs(rrr)+1)*sign(rrr)
par(usr=as.vector(rrr)) 
# Add points, different colours for NA vs EU
points(bioPCs$scores[PopData$Region=="NorthAm",c(PCA,PCB)],col="#F5375166",pch=16)
points(bioPCs$scores[PopData$Region=="Europe",c(PCA,PCB)],col="#4FB0C666",pch=16)

##############################
## 2.  Reduce number of PC variables
##############################
## Show PC loadings
plot(bioPCs)
summary(bioPCs) # Looks like 99% of variation in PC1-11

## Determine which PCs explain significant variation:
# For Methods 1 & 2 see Legendre & Legendre (2012) pg. 449
# Method 1: Broken stick model (Jackson 1993)
E0<-bstick(bioPCs) # Function from Vegan to calculate null eigenvalues
E1<-bioPCs$sdev^2

# Method 1a (Not Used): Retain PCs with eigenvalues > broken stick model (i.e. null mocel)
screeplot(bioPCs,bstick=T,type="lines")
keepPCs<-data.frame(bioPCs$scores[,E1>E0])
ncol(keepPCs) # Number of PCs retained

# Method 1b (Not Used): Retain PCs with cumulative eigenvalues > broken stick model
# Null variance
VarT0<-{}
# Observed variance
VarT<-{}
for(i in 1:length(E1)){
  VarT0<-c(VarT0,sum(E0[1:i]))
  VarT<-c(VarT,sum(E1[1:i]))
}
keepPCs<-data.frame(bioPCs$scores[,VarT>VarT0])
ncol(keepPCs) # Number of PCs retained

# Method 2 (Kaiser-Guttman criterion) (Not Used): Retain PCs with eigenvalues > average eigenvalue
keepPCs<-data.frame(bioPCs$scores[,E1>mean(E1)])
ncol(keepPCs) # Number of PCs retained

## Method 3: Retain PCs that are correlated with fitness components
# Function to choose PCs based on statistical association with response variable
PCpick<-function(Trait="Fruits",PCs=bioPCs$scores,logT=T,iter=1000){
  # Standardize Response variable
  Resp<-PopData[,Trait]
  if(logT==T){Resp<-log(Resp+1)} # Log transform if LogT==T
  Resp<-(Resp-mean(Resp,na.rm=T))/sd(Resp,na.rm=T)
  # Calculate r2 between each PC and 'Trait'
  rPCs<-data.frame(PC=1:ncol(PCs),r=apply(PCs,2,function(x) cor(x,Resp,use="complete.obs")))
  rPCs$Rsq<-rPCs$r^2
  # Randomization test (null expectation)
  Null<-{}
  for(i in 1:iter){
    Null<-rbind(Null,apply(PCs[sample(1:nrow(PCs)),],2,function(x) cor(x,Resp,use="complete.obs")^2))
  }
  ## Find eigenvectors that are more strongly correlated with the Response variable than expected by chance:
  # Order null simulations
  Null<-apply(Null,2,sort)
  rPCs$Null<-Null[ceiling(iter*0.95),]
  # Rank from highest to lowest 
  rPCs<-rPCs[order(rPCs$Rsq,decreasing=T),]
  rPCs$Rank<-1:ncol(PCs)
  # Plot Rsq with red line showing null model
  g<-qplot(x=Rank,y=Rsq,data=rPCs,geom="bar",stat="identity")+theme_classic()+geom_text(aes(x=Rank,y=Rsq,label=PC),vjust=-0.5)
  g<-g+geom_smooth(aes(x=Rank,y=Null),colour="red",se=F)
  print(g)
  # Vector of PCs to keep
  keep<-rPCs$PC[rPCs$Rsq>rPCs$Null]
  keepPCs<-data.frame(PCs[,as.numeric(keep)])
  names(keepPCs)<-paste0("PC",keep)
  return(cbind(Resp,keepPCs))
}
# Keep Predictive PCs
rFruits<-PCpick(Trait="Fruits",logT=T)
summary(lm(Resp~.,data=rFruits))

rTotalDens<-PCpick(Trait="TotalDens",logT=T)
summary(lm(Resp~.,data=rTotalDens))

rRosRatio<-PCpick(Trait="RosRatio",logT=F)
summary(lm(Resp~.,data=rRosRatio))

rPopSize<-PCpick(Trait="Pop_Size",logT=T)
summary(lm(Resp~.,data=rPopSize))

#### CONTINUE HERE###
### ADD rPerfIndex to this script...

# Number of PCs retained:
length(grep("PC",names(rFruits)))
length(grep("PC",names(rTotalDens)))
length(grep("PC",names(rRosRatio)))
length(grep("PC",names(rPopSize)))

### NOTE: there is also a null expectation! (but bootstrap more reliable for non-normal data)
## See probability density function for Pearson product-moment correlation:
## http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient

##############################
## 3.  Regression on PC axes (with spatially autocorrelated errors)
############################## 
# Function for multiple regression with spatially autocorrelated error
mreg<-function(Data=rFruits,sort=F){
  # Add spatial dimensions and Region for analysis
  Data<-cbind(Data,PopData[,c("Longitude","Latitude","Region")])
  # Remove missing observations (i.e. response variable or coordinates)
  Data<-Data[complete.cases(Data),]
  # Remove duplicated Lat/Long
  Data<-Data[!duplicated(Data[,c("Longitude","Latitude")]),]
  
  # Formula for the model
  mod<-paste(paste0("Resp~-1"),paste(names(Data)[grep("PC",names(Data))],collapse="+"),
             #paste0("I(",names(Data)[grep("PC",names(Data))],"^2)",collapse="+"),
             sep="+")
  Model<-gls(formula(mod),data=Data,
             correlation=corExp(form=~Longitude+Latitude|Region),na.action=na.exclude)
  Pred<-data.frame(fit=Model$fitted,obs=Model$fitted+Model$residuals,Region=Model$groups)
  Pred$Region<-factor(Pred$Region,levels=c("NorthAm","Europe"))
  MOut<-data.frame(coef=round(coefficients(Model),3),F=round(anova(Model)$F,2),p=round(anova(Model)$p,3))
  # Sort coefficients by highest to lowest loadings?
  if(sort==T){MOut<-MOut[order(abs(MOut$coef),decreasing=T),]}
  return(list(fit=Pred,coef=MOut))
}

# Fruits
Fruits<-mreg(Data=rFruits)
qplot(fit,obs,data=Fruits$fit,colour=Region)+geom_smooth(method="lm")+theme_classic()
summary(lm(obs~fit,data=Fruits$fit))

# Total Density
TotalDens<-mreg(Data=rTotalDens)
qplot(fit,obs,data=TotalDens$fit,colour=Region)+geom_smooth(method="lm")+theme_classic()
summary(lm(obs~fit,data=TotalDens$fit))

# Rosette Ratio
RosRatio<-mreg(Data=rRosRatio)
qplot(fit,obs,data=RosRatio$fit,colour=Region)+geom_smooth(method="lm")+theme_classic()
summary(lm(obs~fit,data=RosRatio$fit))

# Population Size
PopSize<-mreg(Data=rPopSize)
qplot(fit,obs,data=PopSize$fit,colour=Region)+geom_smooth(method="lm")+theme_classic()
summary(lm(obs~fit,data=PopSize$fit))

############################## 
# 4. Back-transformation of loadings from regression and PCA to identify key bioclim variables
############################## 
## Details in Legendre & Legendre (2012) pp 562-563
U<-as.data.frame(loadings(bioPCs)[,1:ncol(loadings(bioPCs))]) # Eigenvectors from original PCA
names(U)<-paste0("PC",1:ncol(loadings(bioPCs)))
## Calculate coefficients of original bioclim variables
# 1. Calculate matrix containing coefficients from PC regression (i.e. each ROW is a PC)
C<-data.frame(Fruits=1:ncol(loadings(bioPCs))*0,TotalDens=1:ncol(loadings(bioPCs))*0,
              RosRatio=1:ncol(loadings(bioPCs))*0,PopSize=1:ncol(loadings(bioPCs))*0)
C$Fruits[as.numeric(gsub("PC","",row.names(Fruits$coef)))]<-Fruits$coef$coef
C$TotalDens[as.numeric(gsub("PC","",row.names(TotalDens$coef)))]<-TotalDens$coef$coef
C$RosRatio[as.numeric(gsub("PC","",row.names(RosRatio$coef)))]<-RosRatio$coef$coef
C$PopSize[as.numeric(gsub("PC","",row.names(PopSize$coef)))]<-PopSize$coef$coef

# 2. Multiply U by each column in C to obtain coefficients for original bioclim variables (i.e. each ROW is a BIOCLIM variable)
B<-data.frame(Fruits=as.matrix(U) %*% C$Fruits,
              TotalDens=as.matrix(U) %*% C$TotalDens,
              RosRatio=as.matrix(U) %*% C$RosRatio,
              PopSize=as.matrix(U) %*% C$PopSize)

write.csv(B,"ClimLoadings.csv",row.names=T)

# Use B to calculate predictions for each performance measurement, and save to main dataset
PopData$PCFruits<-apply(scale(PCs),1,function(x) sum(x*B$Fruits))
PopData$PCTotalDens<-apply(scale(PCs),1,function(x) sum(x*B$TotalDens))
PopData$PCRosRatio<-apply(scale(PCs),1,function(x) sum(x*B$RosRatio))
PopData$PCPopSize<-apply(scale(PCs),1,function(x) sum(x*B$PopSize))

qplot(y=log(Fruits+1),x=PCFruits,colour=Region,data=PopData)+theme_classic()+geom_smooth(method="lm",se=F)
qplot(y=log(TotalDens+1),x=PCTotalDens,colour=Region,data=PopData)+theme_classic()+geom_smooth(method="lm",se=F)
qplot(y=RosRatio,x=PCRosRatio,colour=Region,data=PopData)+theme_classic()+geom_smooth(method="lm",se=F)
qplot(y=log(Pop_Size+1),x=PCPopSize,colour=Region,data=PopData)+theme_classic()+geom_smooth(method="lm",se=F)


################
## TO DO:
# PROBLEM: How to quantify similarity/difference of pairs?
# Compare angle
# How to compare specific elements (e.g. bio1)?
# NEED A NULL DISTRIBUTION

# Calculate correlations for each 'PC'
rPCs<-data.frame(Fruits=apply(PCs,2,function(x) cor(x,PopData$PCFruits)),
                     TotalDens=apply(PCs,2,function(x) cor(x,PopData$PCTotalDens)),
                     RosRatio=apply(PCs,2,function(x) cor(x,PopData$PCRosRatio)),
                     PopSize=apply(PCs,2,function(x) cor(x,PopData$PCPopSize)))

# Compare similarity (i.e. angles, in degrees) between matrices
### TO DO: Make function to input B (coefficients) or rPCs (R)
theta<-function(a=NA,b=NA){
  return(acos(sum(a*b)/(sqrt(sum(a^2))*sqrt(sum(b^2))))*180/pi)
}
angles<-function(InData=rPCs){
  Angles<-data.frame()
  for (i in 1:(ncol(InData)-1)){
    for(j in (i+1):ncol(InData)){
      Angles<-rbind(Angles,data.frame(
        Comparison=paste(names(InData)[i],"x",names(InData)[j]),
        Angle=round(theta(InData[,i],InData[,j]),1),
        r=round(cor(InData[,i],InData[,j]),3)
      ))
    }
  }  
  return(Angles)
}
# Show similarity:
# Angles based on coefficients estimated from PC regression
# Should be interpreted cautiously when variables are inter-correlated
angles(B)
# Angles based on correlation coefficients
angles(rPCs)

## Plot eigenvectors using colour-coded vectors
categ<-data.frame(cat=1:35*NA,clr=1:35*NA)
row.names(categ)<-paste0("bio",1:35)
# Temperature
categ$cat[c(1,5,6,8,9,10,11)]<-"T"
categ$clr[c(1,5,6,8,9,10,11)]<-"#D72638"
categ$cat[c(2,4)]<-"T_Var"
categ$clr[c(2,4)]<-"#D7263866"

# Precipitation
categ$cat[c(12,13,14,16,17,18,19)]<-"Pcp"
categ$clr[c(12,13,14,16,17,18,19)]<-"#51BBFE"
categ$cat[15]<-"Pcp_Var"
categ$clr[15]<-"#51BBFE66"

# Moisture
categ$cat[c(28,29,30,32,33,34,35)]<-"Moist"
categ$clr[c(28,29,30,32,33,34,35)]<-"#8FF7A7"
categ$cat[31]<-"Moist_Var"
categ$clr[31]<-"#8FF7A766"

# Radiation
categ$cat[c(20,21,22,24,25,26,27)]<-"Rad"
categ$clr[c(20,21,22,24,25,26,27)]<-"#F9C80E"
categ$cat[23]<-"Rad_Var"
categ$clr[23]<-"#F9C80E66"

barplot<-function(Trait="Fruits",abs=F,type=c("r","Coef","Legend")){
  X<-NA
  if(type=="R"){X<-merge(rPCs,categ,by=0)}
  if(type=="Coef"){X<-merge(B,categ,by=0)}
  if(type=="Legend"){
    X<-unique(merge(B,categ,by=0)[,c("cat","clr")])
    X$Row.names<-X$cat
    X$Loading<-10
    Trait="Loading"
  }
  X<-X[order(X[,Trait]^2,decreasing=F),]
  ofst<--max(abs(X[,Trait]))/5
  loads<-X[,Trait]
  if(abs==T){loads<-abs(loads)} else {ofst<-min(loads)+ofst}
  g<-qplot(y=loads,x=1:nrow(X),fill=I(X$clr),stat="identity",geom="bar")+coord_flip()+theme_classic()+ylab(type)+xlab("")+
    geom_text(aes(x=1:nrow(X),y=1:nrow(X)*0+ofst,label=X$Row.names),hjust=0)+scale_x_continuous(breaks=-2,labels="")+ggtitle(Trait)
  return(g)
}

# Plot coeficients (i.e. B)
barplot(Trait="Fruits",abs=F,type="Coef")
barplot(Trait="TotalDens",abs=F,type="Coef")
# Plot correlation coefficients
barplot(Trait="Fruits",abs=F,type="R")
barplot(Trait="TotalDens",abs=F,type="R")

# Show legend
barplot(Trait="Loading",type="Legend",abs=T)

# Save Population means with PCRegression Predictions
write.csv(PopData,"PCData.csv",row.names=F)
