\name{Delaunay4BestMatches}
\alias{Delaunay4BestMatches}
\title{ Delaunay4BestMatches(BestMatches, MatrixOrSize, IsToroid,PlotIt) Calculates the adjacency matrix of the delaunay graph for bestmatches in tiled form if BMs are located on a toroid grid }
\arguments{
\item{BestMatches[1:n,1:3]}{n by 3 matrix containing the BMKey, X and Y coordinates of the n BestMatches BestMatches NEED NOT BE UNIQUE! however, there is an edge in the Deaunay between duplicate points!}
\item{MatrixOrSize[2]}{Defalut 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches}
\item{IsToroid}{logical, indicating if BM's are on a toroid grid. Default is True}
\item{PlotIt}{Set PlotIt=TRUE, if you want to see the Plots}
}
\value{a list of:
  \item{Delaunay[1:n,1:n]}{adjacency matrix of the Delaunay-Graph}
  \item{TiledDelaunay}{if IsToroid,  this is the Tiled Delaunay Graph}
  \item{X, Y}{x- and y-coordinates of the tiled bestmatches (or just b}
}
\description{
 Delaunay4BestMatches(BestMatches, MatrixOrSize, IsToroid,PlotIt) Calculates the adjacency matrix of the delaunay graph for bestmatches in tiled form if BMs are located on a toroid grid }
\author{ALU / Michael Thrun    Aug.2014}
