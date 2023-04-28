PlotDelaunay4BestMatches <- function(BestMatches,MatrixOrSize=c(50,82),Cls=BestMatches[,1]*0+1,IsToroid=1){

# Zeichnen des Delaunay Graphen, Punkte ggf. durch Cls gefaerbt
# INPUT
# BestMatches(1:d,1:3)        Bestmatches
# 
# OPTIONAL
# MatrixOrSize[2]    Default 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches
# Cls(1:d)           Classes of bestmatches  or []
# IsToroid           ==1 (default) => Randkorrigiertes Universum
#     
# USES   Delaunay4BestMatches(BestMatches,MatrixOrSize,IsToroid);
#
# Author: RG Nov.2014
  

  DBM <- Delaunay4BestMatches(BestMatches, MatrixOrSize, IsToroid,PlotIt=TRUE)
  par(new = TRUE)
  
  X <- DBM$X
  Y <- DBM$Y
  xlim <- c(min(X),max(X))
  ylim <- c(min(Y),max(Y))
  
  if(IsToroid){
    Cls <- c(Cls,Cls,Cls,Cls)
    ClassPlot(X,Y,Cls ,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
  }
  else{
    ClassPlot(X,Y,Cls ,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
  }


}

## Groesse der U-Matrix feststellen
#if(is.vector(MatrixOrSize)&length(MatrixOrSize)==2){ # MatrixOrSize=c(..,..) 
#  Lines   = MatrixOrSize[1]
#  Columns = MatrixOrSize[2]
#}else{ # MatrixOrSize=0 Matrix
#  Columns = ncol(MatrixOrSize)
#  Lines   = nrow(MatrixOrSize)
#}# end(is.vector(MatrixOrSize)&length(MatrixOrSize)==2)
## jetzt gibts Lines und Columns
#

## ggf kacheln
#if (IsTiled  == 1){
#  DBM <- Delaunay4BestMatches(BestMatches,cbind(Lines, Columns),1)
#  Delaunay <- DBM$Delaunay
#  TiledDelaunay <- DBM$TiledDelaunay
#  X <- DBM$X
#  Y <- DBM$Y
#  TiledCls <- c(Cls, Cls, Cls, Cls);
#  # delaunay Graph zeichnem
#  
#  xlim <- c(min(X),max(X))
#  ylim <- c(min(Y),max(Y))
#  figure()
#  plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE)    
#  # weisse punkte sieht keiner 
#  axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
#  axis(2,ylim=ylim,col="black",las=1) #und y-Achse
#  
#  par(new = TRUE)
#  ClassPlot(X,Y,TiledCls ,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
#  par(new = TRUE)
#  ClassPlot(X,Y,TiledCls ,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)
#  par(new = TRUE) 
#  gplot(TiledDelaunay,cbind(X,Y),xlim=xlim,ylim=ylim)
#}

#else{
#  Delaunay <- Delaunay4BestMatches(BestMatches,cbind(Lines, Columns),0)$Delaunay
#  X <- BestMatches[,3];
#  Y <- Lines+1-BestMatches[,2]; 
#  
#  xlim <- c(min(X),max(X))
#  ylim <- c(min(Y),max(Y))
#  figure()
#  plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE)  
#  # weisse punkte sieht keiner 
#  axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
#  axis(2,ylim=ylim,col="black",las=1) #und y-Achse
#  
#  # delaunay Graph zeichnen
#  par(new = TRUE)
#  ClassPlot(X,Y,Cls,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
#  par(new = TRUE)
#  ClassPlot(X,Y,Cls,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)   
#  par(new = TRUE)
#  gplot(Delaunay,cbind(X,Y),xlim=xlim,ylim=ylim)
#}