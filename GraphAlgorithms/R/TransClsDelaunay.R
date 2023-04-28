TransClsDelaunay <- function(Delaunay,TRI,Cls){
# calculates all points with Delaunay-Edges crossing into different classes.
#input:#Delaunay: adjacency-matrix of the Delaunay-Graph
# TRI: A n by 3 matrix containing the three points forming a Delaunay-Triangle in rows
#Cls: Vector containing the classes of bestmatches.

transvsinnerpoints <- InnerVsTransDelaunayKanten(Delaunay, Cls); # getting indices of inner points with edges in different classes and those in one class
intransclstri <- rep(0, nrow(TRI)); #
for(i in 1:length(intransclstri)){
intransclstri[i] <- length(intersect(TRI[i,], transvsinnerpoints$TransClsInd)) > 0; # calculate the intersection between triangles and points with edges to other classes, inner triangles are set to 0
}
TransClsTRI <- TRI[which(intransclstri == 1, arr.ind = T),];# all triangles that have at least one point in TransClsInd and are therefore on the edge of a class 

return(list(TransCLSPointInd = transvsinnerpoints$TransClsInd, TransClsTRI = TransClsTRI, TransAInd = transvsinnerpoints$TransAInd, TransBInd = transvsinnerpoints$TransBInd, InnerPointInd = transvsinnerpoints$InnerPointInd))
}