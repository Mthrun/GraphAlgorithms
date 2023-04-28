PlotVoronoiGraph <- function(X, Y, Cls = X*0+1, OnlyRand = 0, isToroid = 1){
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
  Colors=DataVisualizations::DefaultColorSequence
  if(OnlyRand == 1){
    stop("OnlyRand == 1 is not working currently")
    TRI           <- DelaunayGraphMatrix(X,Y)$TRI;
    RandPunktInd  <- TransClsDelaunay(Delaunay,TRI,Cls)$TransCLSPointInd
    RandTRI       <- TransClsDelaunay(Delaunay,TRI,Cls)$TransClsTRI
    TransAInd     <- TransClsDelaunay(Delaunay,TRI,Cls)$TransAInd
    TransBInd     <- TransClsDelaunay(Delaunay,TRI,Cls)$TransBInd
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
    #title('Inter Class Voronoi Graph')
  }else{# Voronoi ueberall zeichnen
    if(isToroid ==1){  # Eingaberaum kacheln
      XPlusCorner <- CornerPoints(X,Y,Cls)$XPlusCorner;
      YPlusCorner <- CornerPoints(X,Y,Cls)$YPlusCorner;
      ClsPlusCorner <- CornerPoints(X,Y,Cls)$ClsPlusCorner;
      CornerX <- CornerPoints(X,Y,Cls)$CornerX;
      CornerY <- CornerPoints(X,Y,Cls)$CornerY;
      ClsCorner <- CornerPoints(X,Y,Cls)$ClsCorner;
      VoronoiRandLimits <- CornerPoints(X,Y,Cls)$VoronoiRandLimits; 
      
      X = XPlusCorner
      Y = YPlusCorner
    }
  }
  
  V        = deldir(X, Y, plot = FALSE, wl = 'tess')
  Graph    = V$dirsgs
  
  #V$dirsgs[1:5,]
  #V$summary[1:5,]
  
  
  NumEdges = dim(Graph)[1]
  
  edge_shapes = list()
  for(i in 1:NumEdges){
    edge_shape = list(type = "line",
                      line = list(color = "#030303", width = 0.3),
                      x0 = Graph[i,1],
                      y0 = Graph[i,2],
                      x1 = Graph[i,3],
                      y1 = Graph[i,4])
    edge_shapes[[i]] <- edge_shape
  }
  
  #----------------------------------------------------------------------------#
  # Plotting
  
  MyPlot = plot_ly()
  MyPlot = plotly::add_markers(p = MyPlot, x = X, y = Y, mode = "markers",
                               marker = list(color = Colors[Cls]), type = "scatter")
  XAxis   = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range(min(X), max(X)))
  YAxis   = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range(min(Y), max(Y)))
  MyPlot = layout(p = MyPlot, title = "Voronoi Cells",
                  shapes = edge_shapes,
                  xaxis = XAxis, yaxis = YAxis)
  return(list("VoronoiCells" = V$dirsgs,
              "VoronoiPlot" = MyPlot))
}
