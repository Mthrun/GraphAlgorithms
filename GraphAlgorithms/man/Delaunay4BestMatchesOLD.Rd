\name{Delaunay4BestMatchesOLD}
\alias{Delaunay4BestMatchesOLD}
\title{Calculates the adjacency matrix of the delaunay graph for bestmatches in tiled form if BMs are located on a toroid grid }
\arguments{
\item{BestMatches[n,3]}{n by 3 matrix containing the BMKey, X and Y coordinates of the n BestMatches}
\item{MatrixOrSize[2]}{A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches}
\item{IsToroid}{logical, indicating if BM's are on a toroid grid. Default is True}
}
\value{Delaunay                        adjacency matrix of the Delaunay-Graph}
\description{
 Delaunay4BestMatches(BestMatches, MatrixOrSize) Calculates the adjacency matrix of the delaunay graph for bestmatches in tiled form if BMs are located on a toroid grid }
