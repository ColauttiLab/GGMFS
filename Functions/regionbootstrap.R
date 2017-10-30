# Bootstrap function to test mean difference between regions
regionbootstrap<-function(BData,BVar="Fruits",NIter=10000){
  BData<-BData[,c(BVar,"Region")]
  RegMeans<-tapply(BData[,BVar],list(BData$Region),mean,na.rm=T)
  RegDiff<-RegMeans[["NorthAm"]]-RegMeans[["Europe"]]
  for (i in 1:NIter){
    BData$Region<-sample(BData$Region,replace=F)
    tmpRegMeans<-tapply(BData[,BVar],list(BData$Region),mean,na.rm=T)
    tmpRegDiff<-tmpRegMeans[["NorthAm"]]-tmpRegMeans[["Europe"]]
    RegDiff<-c(RegDiff,tmpRegDiff)
  }
  # Count number of permutations where difference (tmpRegDiff) > observed difference (i.e. RegDiff)
  PVal<-sum(abs(RegDiff[2:1001])>abs(RegDiff[1]))/1000 # Using absolute values (i.e. a 2-tailed test)
  PVal<-round(PVal,3)
  if(PVal==0) PVal<-paste0("<",1/NIter)
  return(data.frame(MeanDiff=signif(RegDiff[1],3),P=PVal))
}