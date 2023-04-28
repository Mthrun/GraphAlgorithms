PlotGraphWithAdjacencyAndProjection = function(Adjacency, ProjectedPoints,
                                               Cls = NULL, Colors = NULL, Title = NULL){
  
  if(!is.numeric(ProjectedPoints) | !is.matrix(ProjectedPoints)){
    stop("ProjectedPoints must be numeric matrix.")
  }
  if(dim(ProjectedPoints)[2] != 2){
    stop("ProjectedPoints must be numeric matrix with two columns.")
  }
  if(!is.numeric(Adjacency) | !is.matrix(Adjacency)){
    stop("Adjacency must be numeric matrix.")
  }
  if(dim(Adjacency)[1] != dim(Adjacency)[2]){
    stop("Adjacency must be numeric symmetric matrix.")
  }
  
  DIM         = dim(Adjacency)[1]
  
  if(is.null(Cls)){
    Cls = rep(1, DIM)
  }
  
  NumCls = length(unique(Cls))
  
  if(is.null(Colors)){
    Colors = DefaultColorSequence[1:NumCls]
  }
  if(is.null(Title)){
    Title = ""
  }
  
  Edges       = Edges_from_adjacency(Adjacency, ProjectedPoints)
  NumEdges    = dim(Edges)[1]
  edge_shapes = list()
  for(i in 1:NumEdges){
    edge_shape = list(type = "line",
                      line = list(color = "#030303", width = 0.3),
                      x0 = Edges[i,1],
                      y0 = Edges[i,2],
                      x1 = Edges[i,3],
                      y1 = Edges[i,4])
    edge_shapes[[i]] <- edge_shape
  }
  X = ProjectedPoints[,1]
  Y = ProjectedPoints[,2]
  MyPlot = plot_ly()
  MyPlot = plotly::add_markers(p = MyPlot, x = X, y = Y, mode = "markers",
                               marker = list(color = Colors[Cls]), type = "scatter")
  XAxis   = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range(min(X), max(X)))
  YAxis   = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range(min(Y), max(Y)))
  MyPlot = layout(p = MyPlot, title = Title,
                  shapes = edge_shapes,
                  xaxis = XAxis, yaxis = YAxis)
  
  return(MyPlot)
}