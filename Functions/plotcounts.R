# Function to extract repeated measures and grouping variable for plotting
# Input data must be formatted like RawDataAll.csv - each row is a population with separate columns for count data P<PLOT#><COUNTMEASURE> e.g. P4Ros
plotcounts<-function(x,y,g="Year",Pop="Pop_Code",LogX=F,LogY=F){
  xCol<-c(t(CorData[,grep(paste0("P[0-9]{1,2}",x,"$"),names(CorData))]))
  yCol<-c(t(CorData[,grep(paste0("P[0-9]{1,2}",y,"$"),names(CorData))]))
  gCol<-factor(rep(CorData[,g],each=10))
  Pop<-factor(rep(CorData[,Pop],each=10))
  
  PlotDat<-data.frame(x=xCol,y=yCol,g=gCol,Pop=Pop)
  if(LogX==T){
    PlotDat$x<-log(PlotDat$x+1)
  }
  if(LogY==T){
    PlotDat$y<-log(PlotDat$y+1)
  }
  names(PlotDat)<-c(x,y,g,"Pop_Code")
  return(PlotDat[complete.cases(PlotDat),])
}