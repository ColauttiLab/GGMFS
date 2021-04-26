
# Function to choose PCs based on statistical association with response variable
PCpick<-function(Trait="MeanFruits",PCs=bioPCs$scores,logT=T,iter=1000){
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
  g<-qplot(x=Rank,y=Rsq,data=rPCs)+theme_classic()+geom_text(aes(x=Rank,y=Rsq,label=PC),vjust=-0.5)
  g<-g+geom_smooth(aes(x=Rank,y=Null),colour="red",se=F)
  print(g)
  # Vector of PCs to keep
  keep<-rPCs$PC[rPCs$Rsq>rPCs$Null]
  keepPCs<-data.frame(PCs[,as.numeric(keep)])
  names(keepPCs)<-paste0("PC",keep)
  return(cbind(Resp,keepPCs))
}