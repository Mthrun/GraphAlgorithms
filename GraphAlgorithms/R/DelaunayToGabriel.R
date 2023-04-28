DelaunayToGabriel <- function(delaunay,X,Y,PlotIt=FALSE){
# res <- DelaunayToGabriel(delaunayGraph, X,Y)
# takes a delaunay graph and removes the edges that don't follow the
# empty ball condition => result is a gabriel graph
# INPUT
# delaunay(1:n,1:n)    adjacancy matrix
# X(1:n)                    x coordinates of points
# Y(1:n)                    y coordinates of points
# PlotIt
# OUTPUT
# Gabriel             adjacency matrix of the gabriel graph
# edges               edges of the gabriel graph
# Author: FL
  
#  requireRpackage("igraph")
#  requireRpackage("RcppArmadillo")
  requireNamespace('igraph')
  #path=paste0(SubversionDirectory(),'PUB/dbt/GabrielGraph/src/')
  #suppressWarnings(sourceCpp(paste0(path,"GabrielGraph.cpp")))
  
  adjMat = igraph::graph.adjacency(delaunay)
  edgeList = igraph::get.edgelist(adjMat, names = FALSE)

  brokenEdges = BrokenGabrielEdgesX(cbind(X,Y),edgeList)
  delaunay[which(brokenEdges, arr.ind=T)] = 0

  adjMat = igraph::graph.adjacency(delaunay)
  edgeList = igraph::get.edgelist(adjMat, names = FALSE)

  if(PlotIt){
    figure()
	  gplot(delaunay,cbind(X,Y),Xlabel='x',Ylabel='Y')
	  #plot(edgeList)
  }
  return(list(Gabriel=delaunay,edges=edgeList))
}