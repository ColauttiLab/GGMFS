## Function for basic PCA of bioclim data
## Requires data frame with bioclim variable columns labelled with 'bio'
bioPCA<-function(data=NA,pcs=2){
  # Exclude PCs 3,7 and 36:40 (redundant)
  PCs<-data[,grep("bio",names(data))[1:35][c(-3,-7)]]
  # Reverse PCs 6, 22 and 30 to improve interpretation (e.g. bio5 is max temp; bio6 is min temp)
  # Currently not useful; but included for consistency with other scripts
  # Useful to keep if loadings are extracted
  PCs$bio6<--PCs$bio6
  PCs$bio22<--PCs$bio22
  PCs$bio30<--PCs$bio30
  # Run principal components
  bioPCs<-princomp(PCs,cor=T)
  PCOut<-data.frame(bioPCs$scores[,1:pcs])
  names(PCOut)<-paste0("PC",1:pcs)
  return(PCOut)
}