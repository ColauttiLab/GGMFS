

# Create long-form data frame of plot-level count data 

######
# Details
######
# This function creates a data frame of the number of rosettes and bolts within each plot, where plot corresponds to each row. 
# This function will extract rosette number, bolt number or fungal number on bolts or rosettes. 
# Input data must be formatted like RawDataAll.csv - each row is a population with separate columns for count data P<PLOT#><COUNTMEASURE> e.g. P4Ros



plotcounts<-function(x,y,z=NA,g="Year",Pop="Pop_Code",LogX=F,LogY=F){
  
  #Create a vector specifying the columns that correspond to x and y variables
  Cols=colnames(CorData[,grep(paste0("P[0-9]{1,2}(",x,"|",y,"|",z,")$"),names(CorData))])
  
  #Selecting relevant columns from data. 
  WideSelected<-CorData %>% select(Cols,g,Pop)
  
  #Transposing columns into a wide format, which also include the plot number. 
  PlotDat<-WideSelected %>% pivot_longer(cols=Cols,names_to=c("Plot",".value"),names_pattern="P([0-9]{1,2})([A-z]*)$")
  
  
  if(LogX==T){
    PlotDat$x<-log(PlotDat$x+1)
  }
  if(LogY==T){
    PlotDat$y<-log(PlotDat$y+1)
  }

  #Converting g to character vector if it is year.
  if(g=="Year"){
    PlotDat$Year<-as.character(PlotDat$Year)
  }
  
    return(PlotDat[complete.cases(PlotDat),])
}