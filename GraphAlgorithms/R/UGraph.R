UGraph <- function(Data, Positions) {
#Calculates the UGraph
#
#Input:
# Data: The original data, as a n-by-d-matrix. N data-points in a d-dimensional data-space are the rows.
# Positions: n-by2-Matrix, containing the coordinates in the 2-dimensional output-space. The same as BestMatches[,2:3]
#
#Output:
#Uheights:
#VoronoiMeanPts:
#
X <- Positions[,1];
Y <- Positions[,2];
uniquify <- UniquePoints(Positions);
	if(nrow(uniquify$unique) != length(uniquify$mergeind)){
	warning("Duplicate points can cause problems.");
	}
numberofcases <- nrow(Data);
delaunay <- DelaunayGraphMatrix(X, Y, Tiling = TRUE)$Delaunay;
voronoicells <- tile.list(deldir(uniquify$unique[,1], uniquify$unique[,2])); # getting a list of all unique voronoi-cells
voronoicells <- voronoicells[uniquify$mergeind]; # adding duplicates to match with Delaunay
numberofdelaunay <- colSums(delaunay);
Uheights <- delaunay * DistanceMatrix(Data, method = "euclidean");
uheights4points <- colSums(Uheights) / numberofdelaunay;
Uheights <- Uheights + diag(uheights4points);
aindbind <- which(delaunay * upper.tri(delaunay, diag = F) >0, T);
numberofvoronoimeanpoints <- nrow(aindbind);
VoronoiMeanPts <- matrix(0, nrow = numberofvoronoimeanpoints, 2);
BorderPt1 <- matrix(0, numberofvoronoimeanpoints, 2);
BorderPt2 <- matrix(0, numberofvoronoimeanpoints, 2); 
UheightBorder <- vector(0, numberofvoronoimeanpoints);

	for (i in 1:numberofvoronoimeanpoints) {
	voronoiA <- voronoicells[[aindbind[i,1]]];
	voronoiB <- voronoicells[[aindbind[i,2]]];
	voronoiApoints <- cbind(voronoiA$x, voronoiA$y);
	voronoiBpoints <- cbind(voronoiB$x, voronoiB$y);
	abborderpoints <- matrix(0 , nrow = 0, ncol = 2);	
		for (j in 1:nrow(voronoiApoints)){
			for(k in 1:nrow(voronoiBpoints)){
				if(paste(voronoiApoints[j,]) == paste(voronoiBpoints[k,])){
				abborderpoints <- rbind(abborderpoints, voronoiApoints[j,]);
				}
			}		
		}
		if(nrow(abborderpoints) == 1){
		abborderpoints <- rbind(abborderpoints, abborderpoints);
		}
	VoronoiMeanPts[i,] <- c(mean(abborderpoints[,1]), mean(abborderpoints[,2]));
	BorderPt1[i,] <- abborderpoints[1,];
	BorderPt2[i,] <- abborderpoints[2,];
	UheightBorder[i,] <- Uheights[aindbind[i,1], aindbind[i,2]];
	}
return(list(Uheights = Uheights, VoronoiMeanPts = VoronoiMeanPts, UheightBorder = UheightBorder, BorderPt1 = BorderPt1, BorderPt2 = BorderPt2, Aind = aindbind[,1], Bind = aindbind[,2]))
}