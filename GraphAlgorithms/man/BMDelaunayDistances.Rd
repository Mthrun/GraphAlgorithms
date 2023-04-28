\name{BMDelaunayDistances}
\alias{BMDelaunayDistances}
\title{ calculates the distances of the Delaunay-Graph of the given bestmatches. }
\arguments{
\item{Bestmatches}{A matrix with n rows and 2 columns, containing the x and y coordinates for all n bestmatches, as returned by getbm() from dbt.ESOM}
\item{Data}{a matrix or data.frame with n rows containing the data from which the bestmatches were calculated. nrow(Data) and nrow(BestMatches) are the same and BestMatch[i,] contains the coordinates for the bestmatch for Data[i,].}
\item{DataDist}{a matrix, containing the pairwise distances of data. Important: dist(Data) returns an object of type dist which has to be converted into a vector or matrix}
\item{Tiling}{logical value indicating whether a toroid universe should be simulated. Default is TRUE.}
}
\value{ESOMDelaunayDistances: A adjacency Matrix of the Delaunay Graph, with the Delaunay-Distances of adjacent points instead of 1 and 0 everywhere else}
\description{
 calculates the distances of the Delaunay-Graph of the given bestmatches. }

