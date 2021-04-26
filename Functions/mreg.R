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