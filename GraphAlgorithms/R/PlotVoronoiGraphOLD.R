PlotVoronoiGraphOLD <- function(X,Y,Cls=X*0+1,OnlyRand=0,isToroid=1){
    
    #  Zeichnen der Voronoizellen
    #  INPUT
    #  [X(1:d),Y(1:d)]      Punktkoordinaten
    # 
    #  OPTIONAL
    #  Cls(1:d)           Classes of bestmatches    
    #  OnlyRand           Nur den Delaunay Graphen fuer Randpunkte von Clustern, default: ==0  
    #  isToroid           ==1 (default) => Randkorrigiertes Universum
    #      
    #  OUTPUT
    #  Delaunay            Sparse Vector des Delaunaygraphen in squareform
    #  TRI                 triangles as returned by delaunay(X,Y);
    # 
    #  USES   DelaunayGraphMatrix(),CornerPoints(),
    #         package deldir 
    #   
 
  #if(!require(deldir)){ # ggf package deldir laden
    # Calculates the Delaunay triangulation and the 
    # Voronoi tessellation (with respect to the entire plane) of a planar point set.
  #  install.packages('deldir')
  #  library(deldir)
 # }else{
  #  library(deldir)
 # }# end if(!require(deldir))
  
  xlim <- c(min(X),max(X))
  ylim <- c(min(Y),max(Y))
  figure()
  plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE);     
  # weisse punkte sieht keiner 
  axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
  axis(2,ylim=ylim,col="black",las=1) #und y-Achse

 
  if (OnlyRand ==1){
    TRI <- DelaunayGraphMatrix(X,Y)$TRI;
    RandPunktInd <- TransClsDelaunay(Delaunay,TRI,Cls)$TransCLSPointInd
    RandTRI <- TransClsDelaunay(Delaunay,TRI,Cls)$TransClsTRI
    TransAInd <- TransClsDelaunay(Delaunay,TRI,Cls)$TransAInd
    TransBInd <- TransClsDelaunay(Delaunay,TRI,Cls)$TransBInd
    InnerPunktInd <- TransClsDelaunay(Delaunay,TRI,Cls)$InnerPointInd
    
    XPlusCorner <- CornerPoints(X,Y,Cls)$XPlusCorner;
    YPlusCorner <- CornerPoints(X,Y,Cls)$YPlusCorner;
    ClsPlusCorner <- CornerPoints(X,Y,Cls)$ClsPlusCorner;
    CornerX <- CornerPoints(X,Y,Cls)$CornerX;
    CornerY <- CornerPoints(X,Y,Cls)$CornerY;
    ClsCorner <- CornerPoints(X,Y,Cls)$ClsCorner;
    VoronoiRandLimits <- CornerPoints(X,Y,Cls)$VoronoiRandLimits;
    PlotIt=1; 
    ####Problem: TransClsVoronoi is not implemented
    par(new = TRUE)
    TransClsVoronoi(X,Y,Cls,Delaunay,TRI,PlotIt,VoronoiRandLimits,xlim=xlim,ylim=ylim)
    par(new = TRUE)
    ClassPlot(X,Y,Cls,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
    par(new = TRUE)
    ClassPlot(X,Y,Cls,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)
    title('Inter Class Voronoi Graph')
  }else {# Voronoi ueberall zeichnen
    if (isToroid ==1){  # Eingaberaum kacheln
      XPlusCorner <- CornerPoints(X,Y,Cls)$XPlusCorner;
      YPlusCorner <- CornerPoints(X,Y,Cls)$YPlusCorner;
      ClsPlusCorner <- CornerPoints(X,Y,Cls)$ClsPlusCorner;
      CornerX <- CornerPoints(X,Y,Cls)$CornerX;
      CornerY <- CornerPoints(X,Y,Cls)$CornerY;
      ClsCorner <- CornerPoints(X,Y,Cls)$ClsCorner;
      VoronoiRandLimits <- CornerPoints(X,Y,Cls)$VoronoiRandLimits; 
      par(new = TRUE)
      deldir(XPlusCorner,YPlusCorner,plot=TRUE,wl='tess',xlim=xlim,ylim=ylim); 
      #title('Randkorrigierter Voronoi Graph')
      par(new = TRUE)
      ClassPlot(XPlusCorner,YPlusCorner,ClsPlusCorner,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
      par(new = TRUE)
      ClassPlot(XPlusCorner,YPlusCorner,ClsPlusCorner,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)
    }
    else {# kein tiling: gewoehnlicher voronoi graph
      par(new = TRUE)
      deldir(X,Y,plot=TRUE,wl='tess',xlim=xlim,ylim=ylim)
      par(new = TRUE)
      ClassPlot(X,Y,Cls,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
      #par(new = TRUE)
      #ClassPlot(X,Y,Cls,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)
      title('voronoi(X,Y)')
    }
  }
  }
