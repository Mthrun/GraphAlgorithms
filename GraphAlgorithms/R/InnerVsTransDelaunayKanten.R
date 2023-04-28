InnerVsTransDelaunayKanten <- function(Delaunay,Cls){
# calculates which edges of the Delaunay-Graph lie in one class (inner) and which lie in two classes (trans).
#input:
# Delaunay: the adjacency matrix of the DelaunayGraph
#Cls: The classes of the BestMatches
#
#Output:
# TransClsInd: vector containing the indices of those points which have edges to points in different classes
# TransAInd, TransBInd: vectors containing the indices of points that lie in different classes and are connected by an edge. TransAInd[i] is connected to TransBInd[i] and the two lie in different classes.
#InnerPointInd: vector containing the indices of those points that have only edges to other points in the same class, i.e. all points that are not in TransClsInd

numberofclasses <- length(Cls);
classdifferent <- (matrix(rep(1,numberofclasses), nrow = numberofclasses, ncol = 1) %*% matrix(Cls, nrow = 1, ncol = numberofclasses)) != (matrix(Cls, nrow = numberofclasses, ncol = 1) %*% matrix(rep(1, numberofclasses), nrow = 1, ncol = numberofclasses));
TransClsDelaunay <- Delaunay * classdifferent;
transABind <- which(TransClsDelaunay != 0 , arr.ind = T);
TransClsInd <- unique(c(transABind[,1], transABind[,2]));
InnerPointInd <- setdiff(c(1:numberofclasses), TransClsInd);

return(list(TransClsInd = TransClsInd, TransAInd = transABind[,1], TransBInd = transABind[,2], InnerPointInd = InnerPointInd))
}