KNNGraph=function(DistanceMatrix,k,Data){
# KNNGraphAdjMatrix = KNNGraph(DistanceMatrix,k)
# KNNGraphAdjMatrix = KNNGraph(Data=Data,k)
# Berechnung des KNN  Graphen
#  
# INPUT
# DistanceMatrix(1:n,1:n)               Distanzmatrix
# k                                     Anzahl Naechster Nachbarn
#
# Optional
# Data(1:n,1:n)                         Matrix des Datensatzes, DistanceMatrix ist dann nicht angegeben#
#                                       es werden euklidische Distanzen verwendet
#
# OUTPUT
# KNNGraphAdjMatrix                     Adjazenzmatrix des KNN Graphen

# MT 01/2015
  
#requireRpackage('cccd')
#requireRpackage('igraph')
requireNamespace('cccd')
requireNamespace('igraph')
KNNGraphAdjMatrix=NULL
tryCatch({
if(missing(DistanceMatrix)){
  result=cccd::nng(x = Data, k = k, mutual = TRUE, method = 'Euclidean')
}

if(missing(Data)){
    result=cccd::nng(dx = DistanceMatrix, k = k, mutual = TRUE, method = NULL)
}
  KNNGraphAdjMatrix=igraph::get.adjacency(result,sparse=FALSE,type='both')
},error=function(e){
  warning(paste0('KNNGraphAdjMatrix(): ',e))
  KNNGraphAdjMatrix=matrix(0,nrow(Data),ncol(Data))
})
#a k-nearest neighbor graph is a digraph where each vertex is associated with an observation
# and there is a directed edge between the vertex and it's k nearest neighbors. 
# A mutual k-nearest neighbor graph is a graph where there is an edge between x and y 
# if x is one of the k nearest neighbors of y AND y is one of the k nearest neighbors of x. 
# MT: mutual is required, if shortest paths ar calculated and result has to fullfill triangle inequality
return(KNNGraphAdjMatrix)
}