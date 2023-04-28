FastGabrielGraph <- function(X,Y,PlotIt=FALSE){
  # res <- FastGabrielGraph(X,Y)
  # generates a delaunay graph and removes the edges that don't follow the
  # empty ball condition => result is a gabriel graph
  # INPUT
  # X(1:n)                    x coordinates of points
  # Y(1:n)                    y coordinates of points
  # PlotIt
  # OUTPUT
  # Gabriel             adjacency matrix of the gabriel graph
  # edges               edges of the gabriel graph
  # author: FL
  d <- DelaunayGraphMatrix(X,Y)
  DelaunayToGabriel(d$Delaunay,X,Y, PlotIt=PlotIt)
}