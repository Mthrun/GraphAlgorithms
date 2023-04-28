BMDelaunayDistances<- function(BestMatches,Data, DataDist = as.matrix(dist(Data)), Tiling = TRUE){
# calculates the distances of the Delaunay-Graph of the given bestmatches.
#
#Input:
# Bestmatches: A matrix with n rows and 2 columns, containing the x and y coordinates for all n bestmatches, as returned by getbm() from dbt.ESOM
# Data: a matrix or data.frame with n rows containing the data from which the bestmatches were calculated. nrow(Data) and nrow(BestMatches) are the same and BestMatch[i,] contains the coordinates for the bestmatch for Data[i,].
# DataDist: a matrix, containing the pairwise distances of data. Important: dist(Data) returns an object of type dist which has to be converted into a vector or matrix 
# Tiling: logical value indicating whether a toroid universe should be simulated. Default is TRUE.
#Output:
#ESOMDelaunayDistances: A adjacency Matrix of the Delaunay Graph, with the Delaunay-Distances of adjacent points instead of 1 and 0 everywhere else
# BMDelaunayDists: vector containing all Delaunay-Distances != 0
# DataDist: vector containing the distances between rows of Data
#Delaunay: adjacency matrix of the Delaunay-Graph
#TRI: A n b 3 Matrix containing the BestMatch-Indices of Delaunay-Triangles in rows

bmx <- BestMatches[,1]; # separating x and y coordinates of the bestmatches
bmy <- BestMatches[,2]; 
del <- DelaunayGraphMatrix(bmx, bmy, Tiling); # calculating the Delaunay-Graph
TRI <- del$TRI; # A n by 3 matrix containing the indices of the points forming a Delaunay-Triangle in every row.
Delaunay <- del$Delaunay; # The adjacency matrix of the Delaunay-Graph
delv <- as.vector(Delaunay); # making a vector out of the Matrix
distv <- as.vector(as.matrix(dist(Data))); # making a full matrix out of the distances, then making a vector out of it.
ESOMDelaunayDistances <- delv*distv; # adjacency vector and distance vector are multiplied componentwise creating a vector containing the distances between adjacent bestmatches and 0 in all other places
ESOMDelaunayDistances <- matrix(ESOMDelaunayDistances, nrow = nrow(Delaunay), ncol = ncol(Delaunay), byrow = T); # turning it into a adjacency matrix containing the dDelaunay-Distances instead of 1 for adjacent points. 
BMDelaunayDists <- ESOMDelaunayDistances[which(lower.tri(ESOMDelaunayDistances), arr.ind = T)]; # the lower triangular matrix of distances as a vector
BMDelaunayDists <- BMDelaunayDists[which(BMDelaunayDists != 0, arr.ind = T)]; # just the Distances

return(list(ESOMDelaunayDistances = ESOMDelaunayDistances, BMDelaunayDists = BMDelaunayDists, DataDist = DataDist, Delaunay = Delaunay, TRI = TRI))
}