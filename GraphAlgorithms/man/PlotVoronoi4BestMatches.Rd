\name{PlotVoronoi4BestMatches}
\alias{PlotVoronoi4BestMatches}
\title{ Zeichnen der Voronoizellen mit Sites, ggf. durch Cls gefaerbt }
\arguments{
\item{Bestmatches(1:d,1:3)}{Bestmatches}
\item{MatrixOrSize[2]}{Default 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches}
\item{Cls(1:d)}{Classes of bestmatches  or []}
\item{OnlyRand}{Nur den Delaunay Graphen fuer Randpunkte von Clustern, default: ==0}
\item{Tiling}{==1 (default) Matrix wird  gekachelt}
}
\value{Delaunay            Sparse Vector des Delaunaygraphen in squareform}
\description{
 Zeichnen der Voronoizellen mit Sites, ggf. durch Cls gefaerbt }

