PlotDelaunay4Points <- function(X,Y,Cls=X*0+1,Index=1:length(X),isToroid=1,OnlyRand=0){
  
  #Zeichnen des DelaunayGraphen nur fuer Punkte X(Index),Y(Index)
  #INPUT
  #X(1:n),Y(1:n)      Punktkoordinaten
  
  #OPTIONAL
  #Cls                Klassifizierung der Punkte
  #Index              Indexmenge fuer  X(Index),Y(Index)
  #isToroid           ==1 => Toroides Universum
  #
  #
  #USES   PlotDelaunayGraph(X,Y,Cls,OnlyRand,isToroid)
  
  X <- X[Index]
  Y <- Y[Index]
  Cls <- Cls[Index]
  
  PlotDelaunayGraph(X,Y,Cls,OnlyRand,isToroid)
    
}
    
  ## Falls PlotDelaunayGraph nicht aufgerufen werden soll:
    
  #if(!require(deldir)){ # ggf package deldir laden
  #  # Calculates the Delaunay triangulation and the 
  #  # Voronoi tessellation (with respect to the entire plane) of a planar point set.
  #  install.packages('deldir')
  #  library(deldir)
  #}else{
  #  library(deldir)
  #}# end if(!require(deldir))

  
  
  #if (isToroid>0){ # Randpunkte erganzen
  #  XPlusCorner <- CornerPoints(X,Y,Cls)$XPlusCorner
  #  YPlusCorner <- CornerPoints(X,Y,Cls)$YPlusCorner
  #  ClsPlusCorner <- CornerPoints(X,Y,Cls)$ClsPlusCorner
  #  CornerX <- CornerPoints(X,Y,Cls)$CornerX
  #  CornerY <- CornerPoints(X,Y,Cls)$CornerY
  #  ClsCorner <- CornerPoints(X,Y,Cls)$ClsCorner
  #  VoronoiRandLimits <- CornerPoints(X,Y,Cls)$VoronoiRandLimits
  #  RandPunkteX = cbind(VoronoiRandLimits[1],VoronoiRandLimits[2],0,0)
  #  RandPunkteY = cbind(0,0,VoronoiRandLimits[3],VoronoiRandLimits[4])
    
  #  xlim <- c(min(c(X,RandPunkteX)),max(c(X,RandPunkteX)))
  #  ylim <- c(min(c(Y,RandPunkteX)),max(c(Y,RandPunkteX)))
  #  figure()
  #  plot(c(X,RandPunkteX),c(Y,RandPunkteY),col='white',xlim=xlim,ylim=ylim,axes=FALSE);     
  #  # weisse punkte sieht keiner 
  #  axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
  #  axis(2,ylim=ylim,col="black",las=1) #und y-Achse
    
  #  X <- XPlusCorner
  #  Y <- YPlusCorner
  #  RandClassNumber <- (max(Cls)+1)*100;
  #  Cls = c(Cls,ClsCorner*0+RandClassNumber);}
  #else{
  #  xlim <- c(min(X),max(X))
  #  ylim <- c(min(Y),max(Y))
  #  figure()
  #  plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE);     
  #  # weisse punkte sieht keiner 
  #  axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
  #  axis(2,ylim=ylim,col="black",las=1) #und y-Achse
  #}
  
  #if (!all(Cls==X*0+1)){ #Cls gegeben
  #  par(new = TRUE)
  #  ClassPlot(X,Y,Cls,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
  #  par(new = TRUE)
  #  ClassPlot(X,Y,Cls,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
  #  Delaunay <- DelaunayGraphMatrix(X,Y)$Delaunay
  #  TRI <- DelaunayGraphMatrix(X,Y)$TRI
  #  if (OnlyRand ==1) {#nur am Rand der Klassen den Delaunay Zeichnen        
  #    RandPunktInd <- TransClsDelaunay(Delaunay,TRI,Cls)$TransCLSPointInd
  #    RandTRI <- TransClsDelaunay(Delaunay,TRI,Cls)$TransClsTRI
  #    TransAInd <- TransClsDelaunay(Delaunay,TRI,Cls)$TransAInd
  #    TransBInd <- TransClsDelaunay(Delaunay,TRI,Cls)$TransBInd
  #    InnerPunktInd <- TransClsDelaunay(Delaunay,TRI,Cls)$InnerPointInd
  #    par(new = TRUE)
  #    ##### x und y nur fuer RandIndizes verwenden, statt gplot?
  #    gplot(RandTRI,cbind(X,Y),xlim=xlim,ylim=ylim);
  #    for (i in 1:length(TransAInd)){
  #      par(new = TRUE)
  #      plot(c(X[TransAInd[i]], X[TransBInd[i]]),c(Y[TransAInd[i]], Y[TransBInd[i]]),xlim=xlim,ylim=ylim,axes=FALSE);
  #    }
  #    par(new = TRUE)
  #    plot(X[InnerPunktInd],Y[InnerPunktInd],xlim=xlim,ylim=ylim,axes=FALSE)
  #  }
  #  else {# Delaunay ueberall zeichnen
  #    par(new = TRUE)
  #    deldir(X,Y,plot=TRUE,wl='triang',xlim=xlim,ylim=ylim)
  #  }
  #}
  #else{ # keine Cls Gegeben
  #  Delaunay <- DelaunayGraphMatrix(X,Y)$Delaunay
  #  TRI <- DelaunayGraphMatrix(X,Y)$TRI
  #  deldir(X,Y,plot=TRUE,wl='triang',xlim=xlim,ylim=ylim)
  #}
  
  #title('randkorrigierter Delaunay Graph')
  