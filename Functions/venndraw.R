########################################
# Function for drawing venn diagrams   #
# with values scaled 0-1               #
########################################
# Function to draw Venn diagram
venndraw<-function(InData=NA){
  # Function to draw circles
  crcl<-data.frame(x=Size*cos(seq(pi,3*pi,length.out=Size*100)),y=Size*sin(seq(pi,3*pi,length.out=Size*100)))
  # Dummy variable for plotting as grid
  dmy<-data.frame(x=c(-500,500),y=c(-500,500))
  # Vector of font sizes
  FSz<-LSz/3+3.5*InData/max(InData)
  P<-ggplot(data=dmy,aes(x=0,y=0))+
    ## Add Circles
    geom_polygon(aes(x,y+sqrt(Ofst^2/2)),data=crcl,fill=Clrs[1],alpha=0.3)+
    geom_polygon(aes(x-Ofst,y-Ofst),data=crcl,fill=Clrs[2],alpha=0.3)+
    geom_polygon(aes(x+Ofst,y-Ofst),data=crcl,fill=Clrs[3],alpha=0.3)+
    geom_polygon(aes(x,y+sqrt(Ofst^2/2)),data=crcl,fill=Clrs[1],alpha=0.3)+
    geom_polygon(aes(x-Ofst,y-Ofst),data=crcl,fill=Clrs[2],alpha=0.3)+
    geom_polygon(aes(x+Ofst,y-Ofst),data=crcl,fill=Clrs[3],alpha=0.3)+
    geom_polygon(aes(x,y+sqrt(Ofst^2/2)),data=crcl,fill=Clrs[1],alpha=0.3)+
    geom_polygon(aes(x-Ofst,y-Ofst),data=crcl,fill=Clrs[2],alpha=0.3)+
    geom_polygon(aes(x+Ofst,y-Ofst),data=crcl,fill=Clrs[3],alpha=0.3)+
    ## Add circle labels
    geom_text(aes(x=0,y=Size+sqrt(Ofst^2/2),label=Labs[1]),colour=Clrs[1],size=LSz,vjust=-0.5)+ ## Top Text
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=-sqrt(Size^2/2)-Ofst,label=Labs[2]),colour=Clrs[2],size=LSz,hjust=1,vjust=1)+ ## Bottom-left text
    geom_text(aes(x=sqrt(Size^2/2)+Ofst,y=-sqrt(Size^2/2)-Ofst,label=Labs[3]),colour=Clrs[3],size=LSz,hjust=0,vjust=1)+ ## Bottom-left text
    ## Add main values
    geom_text(aes(x=0,y=Size+sqrt(Ofst^2/2),label=formatC(InData[1],digits=3,format="f")),size=FSz[1],vjust=1.5)+
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=-sqrt(Size^2/2)-Ofst,label=formatC(InData[2],digits=3,format="f")),size=FSz[2],vjust=-1,hjust=1)+
    geom_text(aes(x=sqrt(Size^2/2)+Ofst,y=-sqrt(Size^2/2)-Ofst,label=formatC(InData[3],digits=3,format="f")),size=FSz[3],vjust=-1,hjust=0)+
    ## Add pairwise values
    geom_text(aes(x=-sqrt(Size^2/2)-Ofst,y=sqrt(Size^2/2)-Ofst,label=formatC(InData[4],digits=3,format="f")),size=FSz[4],vjust=1,hjust=0)+ # 12
    geom_text(aes(x=+sqrt(Size^2/2)+Ofst,y=sqrt(Size^2/2)-Ofst,label=formatC(InData[5],digits=3,format="f")),size=FSz[5],vjust=1,hjust=1)+ #13
    geom_text(aes(x=0,y=-Size-sqrt(Ofst^2/2),label=formatC(InData[6],digits=3,format="f")),size=FSz[6],vjust=0,hjust=0.5)+ # 23
    ## Add 3-way
    geom_text(aes(x=0,y=0,label=formatC(InData[7],digits=3,format="f")),size=FSz[7],vjust=1)+ #123
    xlim(-1.5*Size-sqrt(Ofst^2/2),1.5*Size+sqrt(Ofst^2/2))+ylim(-1.5*Size-sqrt(Ofst^2/2),1.5*Size+sqrt(Ofst^2/2))+theme_map()
  return(P)
}