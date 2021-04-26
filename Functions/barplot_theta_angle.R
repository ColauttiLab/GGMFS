#Functions to generate bar plots of PCA weather data in 9_ClimaticNiceModels_PCReg 

barplot<-function(Trait="Fruits",abs=F,type=c("R","Coef","Legend")){
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
  g<-ggplot()+
    geom_col(aes(y=loads,x=1:nrow(X),fill=I(X$clr)))+
  coord_flip()+theme_classic()+ylab(type)+xlab("")+
    geom_text(aes(x=1:nrow(X),y=1:nrow(X)*0+ofst,label=X$Row.names),hjust=0)+scale_x_continuous(breaks=-2,labels="")+ggtitle(Trait)
  return(g)
}

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