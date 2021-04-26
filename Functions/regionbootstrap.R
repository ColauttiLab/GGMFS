# Bootstrap function to test mean difference between regions
regionbootstrap<-function(BData,BVar="Fruits",NIter=1000){

  NorAM<-BData %>% filter(Region=="NorthAm") %>% select(all_of(BVar)) %>% pull() %>% mean(na.rm=T)
  EU<-BData %>% filter(Region=="Europe") %>% select(all_of(BVar)) %>% pull() %>% mean(na.rm=T)
  
   RegDiff<-NorAM-EU
   #Performing permutation simulation
   SimDiff<-c()
  for (i in 1:NIter){
    BData$Region<-sample(BData$Region,replace=F)
    NorAM<-BData %>% filter(Region=="NorthAm") %>% select(BVar) %>% pull() %>% mean(na.rm=T)
    EU<-BData %>% filter(Region=="Europe") %>% select(BVar) %>% pull() %>% mean(na.rm=T)
    tmpSimDiff<-NorAM-EU
    SimDiff<-c(SimDiff,tmpSimDiff)
  }
   
  # Count number of permutations where difference (SimDiff) > observed difference (i.e. RegDiff)
  # Using absolute values (i.e. a 2-tailed test)
  
  PVal<-sum(abs(SimDiff>RegDiff))/length(SimDiff)
  PVal<-round(PVal,3)
  
  # Formating writting
  if(PVal==0){ PVal<-paste0("< ",1/NIter) }
  else if(PVal<0.01){ PVal<-"< 0.01" }
  else if(PVal<0.05){ PVal<-"< 0.05" }
  else { PVal<-paste0("= ",PVal) }
  
  return(data.frame(MeanDiff=signif(RegDiff[1],3),P=PVal))
}


