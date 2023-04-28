PlotDelaunayGraph <-function(X, Y, Graph = NULL, Cls = NULL, Colors = NULL, OnlyRand = 0,
                             Toroid = TRUE, PlotIt = FALSE){
  
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
  
  #----------------------------------------------------------------------------#
  # Error captching
  
  if(is.null(Cls)){
    Cls = rep(1, length(X))
  }
  
  if(is.null(Colors)){
    Colors = DefaultColorSequence[1:length(unique(Cls))]
  }
  
  
  #----------------------------------------------------------------------------#
  # Computation
  
  if(isTRUE(Toroid)){
    CP                = CornerPoints(X,Y,Cls)
    XPlusCorner       = CP$XPlusCorner
    YPlusCorner       = CP$YPlusCorner
    ClsPlusCorner     = CP$ClsPlusCorner
    CornerX           = CP$CornerX
    CornerY           = CP$CornerY
    ClsCorner         = CP$ClsCorner
    VoronoiRandLimits = CP$VoronoiRandLimits
    RandPunkteX       = cbind(VoronoiRandLimits[1],VoronoiRandLimits[2],0,0)
    RandPunkteY       = cbind(0,0,VoronoiRandLimits[3],VoronoiRandLimits[4])
    
    X                 = XPlusCorner
    Y                 = YPlusCorner
    RandClassNumber   = (max(Cls)+1)*100;
    Cls               = c(Cls,ClsCorner*0+RandClassNumber)
  }
  
  
  V        = deldir(X, Y, plot = FALSE, wl = 'triang')
  Graph    = V$delsgs
  
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
  MyPlot = layout(p = MyPlot, title = "Delaunay Graph",
                   shapes = edge_shapes,
                   xaxis = XAxis, yaxis = YAxis)
  if(isTRUE(PlotIt)){
    print(MyPlot)
  }
  return(list("Delaunay"  = Graph,
              #"TRI"       = TRI,
              "GraphPlot" = MyPlot))
}
