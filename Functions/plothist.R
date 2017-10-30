# A function for plotting histograms using a standard formatting scheme
plothist<-function(xval,PData=PopMeans,Lbls=F,FntSz=24,BWth=0.1,HJust=0,HLoc=0){
  Boot<-regionbootstrap(PData,xval) # Calculate mean difference between regions, and permute P-values
  p<-ggplot(data=PData) + theme_classic(base_size=FntSz) + guides(fill=FALSE) # basic formatting
  if(Lbls==F){p<-p + xlab("") + ylab("")} # T/F Print labels
  # Plot separate histograms for native vs introduced range
  p<-p+geom_area(aes_string(xval,"y=(..count..)/sum(..count..)"),binwidth=BWth,data=PData[PData$Region=="Europe",],colour="#FFFFFF99",fill="#96ADC899",size=2,stat="bin")+
    geom_area(aes_string(x=xval,"y=(..count..)/sum(..count..)"),binwidth=BWth,data=PData[PData$Region=="NorthAm",],colour="#FFFFFF99",fill="#F5375199",size=2,stat="bin")+
    theme(plot.title = element_text(hjust=0.2))+theme(plot.title = element_text(hjust=-0.05,vjust=1))
  if(Boot[1]>1000){Boot[1]<-format(Boot[1], big.mark=",", scientific=FALSE)} #add commas to large numbers for readability
  # Annotate with bootstrap measurements
  p<-p+annotate("text",label=paste("\n   I-N  ",Boot[1],"\n","   P  ",Boot[[2]],sep=""),x=HLoc,y=Inf,hjust=HJust,vjust=1,size=FntSz/4)
  # Add medians
  EUMed<-median(PData[PData$Region=="Europe",xval],na.rm=T)
  NAMed<-median(PData[PData$Region=="NorthAm",xval],na.rm=T)
  p<-p+geom_segment(aes(x=EUMed,xend=EUMed,y=-Inf,yend=Inf),colour="#96ADC8",size=1.5)+#,arrow=arrow(length=unit(0.5,"cm")))+
    geom_segment(aes(x=NAMed,xend=NAMed,y=-Inf,yend=Inf),colour="#F53751",size=1.5)#,arrow=arrow(length=unit(0.5,"cm")))+
  return(p)
}