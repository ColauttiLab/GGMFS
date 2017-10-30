#########################
## This script Makes histograms of Populaiton means
## With Venn Diagrams showing results of model selection
#########################

#########################
## Libraries            #
#########################
library(ggplot2)
library(gridExtra)
# Theme for plotting
source("Functions/theme_map.R")
# Include bootstrap estimates of mean and P
source("Functions/regionbootstrap.R") # Default Parameters: (BData,BVar="Fruits",NIter=10000)
# Data for plotting histograms
source("Functions/plothist.R")
# Data for plotting Venn diagrams
source("Functions/venndraw.R")

#########################
## Data                 #
#########################
PCData<-read.csv("PCData.csv")
VennData<-read.csv("Model_Performance.csv",row.names=1)
VennData<-round(VennData,digits=3)


#######################################
## Setup parameters for Venn diagrams #
#######################################
Clrs<-c("#F53751","#77B6EA","#8EBC7C") # Colours for top, left & right circles
Labs<-c("Range","Abiotic","Biotic") # Labels for circles
Ofst=5 # Circle separation
Size=20 # Circle size
LSz=6 # Base font size

##########################
# Write pdf              #
##########################

#svg("4_Figures/Fig2_HistoVenn.svg",width=8.26772,height=11.6929,onefile=T,pointsize = 4) # Standard A4 is 210mm x 297mm
cairo_pdf("4_Figures/Fig2_HistoVenn.pdf",width=8.26772,height=11.6929,onefile=T,pointsize = 4) # Standard A4 is 210mm x 297mm
  grid.newpage() # Open a new page on grid device 
  pushViewport(viewport(width=unit(200,"mm"),height=unit(287,"mm"),just=c("centre","centre"),layout = grid.layout(nrow=110, ncol=100))) # Margins: left/right:10mm x top/bottom:22mm
  # Add Histograms
  # Fruits plot
  NAMed<-median(PCData[PCData$Region=="Europe","Fruits"],na.rm=T)
  EUMed<-median(PCData[PCData$Region=="NorthAm","Fruits"],na.rm=T)
  pFrt<-plothist(xval="Fruits",PData=PCData,BWth=0.2,HLoc=1,FntSz=18)+ggtitle("Fruits")+xlab("N")+scale_x_log10(limits=c(1,1100))
  print(pFrt, vp = viewport(layout.pos.row=10:34,layout.pos.col=1:75))
  # Total Density
  NAMed<-median(PCData[PCData$Region=="Europe","TotalDens"],na.rm=T)
  EUMed<-median(PCData[PCData$Region=="NorthAm","TotalDens"],na.rm=T)
  pTD<-plothist(xval="TotalDens",PData=PCData,BWth=0.2,HLoc=1,FntSz=18)+ggtitle("Total density")+xlab(expression("N/"*m^"2"*""))+scale_x_log10()
  print(pTD, vp = viewport(layout.pos.row=35:59,layout.pos.col=1:75))
  # Population extent (Area)
  NAMed<-median(PCData[PCData$Region=="Europe","Pop_Size"],na.rm=T)
  EUMed<-median(PCData[PCData$Region=="NorthAm","Pop_Size"],na.rm=T)
  pPSi<-plothist(xval="Pop_Size",PData=PCData,BWth=0.5,FntSz=18)+ggtitle("Population extent")+xlab(expression(""*m^"2"*""))+scale_x_log10()
  print(pPSi, vp = viewport(layout.pos.row=60:84,layout.pos.col=1:75))
  # Rosette Ratio
  NAMed<-median(PCData[PCData$Region=="Europe","RosRatio"],na.rm=T)
  EUMed<-median(PCData[PCData$Region=="NorthAm","RosRatio"],na.rm=T)
  pRoR<-plothist(xval="RosRatio",PData=PCData,BWth=0.8,HLoc=-7.5,FntSz=18)+ggtitle("Rosette ratio")+xlab("log(Ros/Adult)")+xlim(-8,8)
  print(pRoR, vp = viewport(layout.pos.row=85:110,layout.pos.col=1:75))

  ## Add Venn Diagrams using above fuctions
  # Fruits
  InData<-VennData$Fruits
  print(venndraw(InData=InData), vp = viewport(layout.pos.row=7:32,layout.pos.col=60:100,y=unit(25,"mm"),clip=F))
  # Total Density
  InData<-VennData$TotalDens
  print(venndraw(InData=InData), vp = viewport(layout.pos.row=30:55,layout.pos.col=60:100))
  # Pop Size
  InData<-VennData$Pop_Size
  print(venndraw(InData=InData), vp = viewport(layout.pos.row=55:80,layout.pos.col=60:100))
  # Rosette Ratio
  InData<-VennData$RosRatio
  print(venndraw(InData=InData), vp = viewport(layout.pos.row=80:105,layout.pos.col=60:100))
dev.off()

