#################################
## Multi-model selection Functions
#################################
# NOTE: Can take a while to run; find best model
# Use gls to incorporate spatial pseudoreplication
ModSel<-function(Data=PCData,Trait="Fruits",LogT=T,PCIn=3,VarIn=paste0("FA",1:3)){
  require(nlme)
  require(glmulti)
  # Remove duplicated lat/long (duplicate spatial location causes error in gls model)
  SubData<-Data[!duplicated(Data[,c("Longitude","Latitude")]),]
  # Check if trait should be log-transformed
  if(LogT==T){Trait=paste0("log(",Trait,"+1)")}
  # Set formula string (note: interaction terms will be fit by 'glmulti' but aren't needed in the base formula)
  mod<-paste0(Trait,"~1+GDD+Altitude+",paste0(VarIn,collapse="+"),"+",paste0("I(",VarIn,"^2)",collapse="+"))
  
  ###########################################################################
  ##### Currently, glmulti gives an error when passing variables
  ##### Here is a crappy workaround
  assign("mod",mod,envir=.GlobalEnv)  # put the dat in the global env
  assign("SubData",SubData,envir=.GlobalEnv)  # put the dat in the global env
  ###########################################################################  
  
  mod2<-gls(model=formula(mod),data=SubData,correlation=corExp(form=~Longitude+Latitude|Region),na.action=na.exclude) # Add spatial structure

  # Run multi-model selection; use genetic algorithm if > 8 terms
  PCpick<-glmulti(formula(mod2),data=SubData,level=1,fitfunction=gls,method="h",plotty=F,na.action=na.exclude) 
  #if(nchar(gsub(".*\\~1+|[A-z0-9\\.\\(\\)]+","",mod))>8){
  #  if(nchar(gsub(".*\\~1+|[A-z0-9\\.\\(\\)]+","",mod))>14){
  #    PCpick<-glmulti(formula(mod2),data=SubData,level=1,fitfunction=gls,method="g",na.action=na.exclude)   
  #  } else {
  #    PCpick<-glmulti(formula(mod2),data=SubData,level=2,fitfunction=gls,method="g",na.action=na.exclude)   
  #  }
  #} else {
  #  PCpick<-glmulti(formula(mod2),data=SubData,level=2,fitfunction=gls,method="h",na.action=na.exclude)
  #}
  return(PCpick)
  
  ###########################################################################
  #### Clean up from 'crappy workaround' above
  remove(mod,envir=.GlobalEnv) # delete dat again from global env
  remove(SubData,envir=.GlobalEnv) # delete dat again from global env
  ###########################################################################
  
}