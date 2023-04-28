PlotDelaunayGraphOLD <-function(X,Y,Cls=X*0+1,OnlyRand=0,isToroid=1){
  
  #  Zeichnen des DelaunayGraphen
  #  INPUT
  #  X(1:d)
  #  Y(1:d)      Punktkoordinaten
  # 
  #  OPTIONAL
  #  Cls(1:d)           Classes of bestmatches    
  #  OnlyRand           Nur den Delaunay Graphen fuer Randpunkte von Clustern                 
  #  isToroid           ==1 => Toroides Universum
  #  
  #  RETURN
  #  Delaunay[1:n,1:n]       The adjacency matrix of the Delaunay-Graph.
  #  TRI                     NOT JET IMPLEMENTED:  for the Moment TRI == delaunayn(X,Y)
  #  
  #  USES  DelaunayGraphMatrix(X,Y,PlotIt)
  #        CornerPoints(X,Y,Cls)
  #        ClassPlot(X,Y,Cls,Title,Xlabel,Ylabel,xlim,ylim,ColorSequence,ColorSymbSequence,PlotLegend,TilePlots) 
  #        TransClsDelaunay(Delaunay,TRI,Cls)
  #        gplot(AdjacencyMatrix,Coordinates,Xlabel,Ylabel,xlim,ylim,LineSpec)
  #        package deldir
  #  
  #  
    
 # if(!require(deldir)){ # ggf package deldir laden
    # Calculates the Delaunay triangulation and the 
 #   # Voronoi tessellation (with respect to the entire plane) of a planar point set.
 #   install.packages('deldir')
 #   library(deldir)
 # }else{
  #  library(deldir)
 # }# end if(!require(deldir))
  
  if (isToroid>0){ # Randpunkte erganzen
    CP <- CornerPoints(X,Y,Cls)
    XPlusCorner <- CP$XPlusCorner
    YPlusCorner <- CP$YPlusCorner
    ClsPlusCorner <- CP$ClsPlusCorner
    CornerX <- CP$CornerX
    CornerY <- CP$CornerY
    ClsCorner <- CP$ClsCorner
    VoronoiRandLimits <- CP$VoronoiRandLimits
    RandPunkteX = cbind(VoronoiRandLimits[1],VoronoiRandLimits[2],0,0)
    RandPunkteY = cbind(0,0,VoronoiRandLimits[3],VoronoiRandLimits[4])
    
    xlim <- c(min(c(X,RandPunkteX)),max(c(X,RandPunkteX)))
    ylim <- c(min(c(Y,RandPunkteX)),max(c(Y,RandPunkteX)))
    figure()
    plot(c(X,RandPunkteX),c(Y,RandPunkteY),col='white',xlim=xlim,ylim=ylim,axes=FALSE);     
    # weisse punkte sieht keiner 
    axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
    axis(2,ylim=ylim,col="black",las=1) #und y-Achse
    box()
    
    X <- XPlusCorner
    Y <- YPlusCorner
    RandClassNumber <- (max(Cls)+1)*100;
    Cls = c(Cls,ClsCorner*0+RandClassNumber)
  }else{
    xlim <- c(min(X),max(X))
    ylim <- c(min(Y),max(Y))
    figure()
    plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE);     
    # weisse punkte sieht keiner 
    axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
    axis(2,ylim=ylim,col="black",las=1) #und y-Achse
    box()
  }
  
  if(!all(Cls==X*0+1)){ #Cls gegeben
    par(new = TRUE)
    ClassPlot(X,Y,Cls,0,'o',0,xlim=NULL,ylim=NULL,PlotLegend=0)
    Del      <- DelaunayGraphMatrix(X,Y)
    Delaunay <- Del$Delaunay
    TRI      <- Del$TRI
    if(OnlyRand == 1) {
      #nur am Rand der Klassen den Delaunay Zeichnen   
      TCL           <- TransClsDelaunay(Delaunay,TRI,Cls)
      RandPunktInd  <- TCL$TransCLSPointInd
      RandTRI       <- TCL$TransClsTRI
      TransAInd     <- TCL$TransAInd
      TransBInd     <- TCL$TransBInd
      InnerPunktInd <- TCL$InnerPointInd
      par(new = TRUE)
      gplot(RandTRI,cbind(X,Y),xlim=xlim,ylim=ylim);
      
      for (i in 1:length(TransAInd)){
        par(new = TRUE)
        plot(c(X[TransAInd[i]], X[TransBInd[i]]),c(Y[TransAInd[i]], Y[TransBInd[i]]),xlim=xlim,ylim=ylim,axes=FALSE);
      }
      par(new = TRUE)
      plot(X[InnerPunktInd],Y[InnerPunktInd],xlim=xlim,ylim=ylim,axes=FALSE)
    }else {# Delaunay ueberall zeichnen
      par(new = TRUE)
      deldir(X,Y,plot=TRUE,wl='triang',xlim=xlim,ylim=ylim)
    }
  }else{ # keine Cls gegeben
    figure()
    Del      <- DelaunayGraphMatrix(X,Y)
    Delaunay <- Del$Delaunay
    TRI      <- Del$TRI
    deldir(X,Y,plot=TRUE,wl='triang',xlim=xlim,ylim=ylim)
  }
  return(list(Delaunay=Delaunay, TRI=TRI))
}
