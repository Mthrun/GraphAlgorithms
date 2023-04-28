BMZwischenPunkte <- function(Delaunay,BestMatches,Data,DataDist = as.matrix(dist(Data))){
# Calculating the middle points of Delaunay-Lines
#input:
# Delaunay: Adjacency matrix of the Delaunay-Graph
#BestMatches: n by 2 Matrix containing the x and y coordinates of the n bestmatches in rows
# Data: The n by m Matrix containing the n points of Data from which the bestmatches are calculated. BestMatch[n,] contains the bestmatch of Data[n,]
# DataDist: The pairwise distances of all rows in Data as a full matrix
# output:
# BMab: The coordinates of the points on the Delaunay-Line between points a and b
# AInd, BInd: The indices of the endpoints a and b
# DataAB: data center of the Delaunay-Line A B
#DataDistAB:
numberofcases <- nrow(BestMatches);
delworkcopy <- Delaunay; # a copy of the adjacency matrix
delworkcopy[which(lower.tri(delworkcopy, diag = T), arr.ind = T)] = 0; # since the adjacency matrix is symmetrical, we only need the upper triangle and set the rest to 0
adjacentpoints <- which(delworkcopy != 0, arr.ind = T); # returns a matrix with two columns, each row containing the BestMatch-indices of two adjacent points.
AInd <- adjacentpoints[,1]; # two vectors of those indices AInd[i] and BInd[i] are indices of adjacent points
BInd <- adjacentpoints[,2]; #
BMa <- BestMatches[AInd,]; # getting the coordinates of points a and b that are connected in the Delaunay-Graph
BMb <- BestMatches[BInd,]; #
BMab <- 0.5 * (BMa + BMb); # adding the coordinates of point a and b and dividing them by 2, getting the coordinates of the middle point on the line between a and b
DataAB <- 0.5*(Data[AInd,] + Data[BInd, ]); #  otherwise the middle is calculated as with the coordinates
DataDistAB <- rep(0, length(AInd)); # initializing the vector
for (i in 1: length(AInd)){
DataDistAB[i] <- DataDist[AInd[i],BInd[i]]; # filling the vector
}
return(list(BMab = BMab, AInd = AInd, BInd = BInd, DataAB = DataAB, DataDistAB = DataDistAB))
}