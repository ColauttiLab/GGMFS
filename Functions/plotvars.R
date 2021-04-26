
# Create long-form data frame of individual plant traits. 

######
# Details
######
# This function  extracts specific data corresponding to plants within populations and convert this to a long format with a grouping variable for graphing.
# This function creates a data frame on the individual rosette and bolt data, taken at 20cm apart (5 individuals) in each plot (10 plots) (i.e., maximum 50 measurements per population - depending on how many plots were included). 
# It then removes all rows with NAs in the data. 
# Input data must be formatted like RawDataAll.csv - each row is a population with separate columns with measurements P<PLOT#><MEASURE><POS#> e.g. P4Adult20
# x and y are plant traits that are to be selected from columns. z is an optional additional trait if three are needed. 

######
# Function
######

plotvars<-function(x,y,z=NA,g="Year",Pop="Pop_Code",LogX=F,LogY=F){
  
  #Create a vector specifying the columns that correspond to x and y variables
  Cols=colnames(CorData[,grep(paste0("P[0-9]{1,2}(",x,"|",y,"|",z,")[0-9]{1,2}"),names(CorData))])
  
  #Selecting relevant columns from data. 
  WideSelected<-CorData %>% select(Cols,g,Pop)
  
  #Transposing columns into a wide format, which also include the plot and plot location of the sampled plant. 
  PlotDat<-WideSelected %>% pivot_longer(cols=Cols,names_to=c("Plot",".value","Meter"),names_pattern="P([0-9]{1,2})([A-z]*)([0-9]{1,2})")
  
  #Logging data if needed. 
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
  
  #Remove missing data and returning data frame
    return(PlotDat[complete.cases(PlotDat),])
}


