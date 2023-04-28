\name{Uheight4Voronoi}
\alias{Uheight4Voronoi}
\title{
Ordnet Voronoigrenzflaechen Hoehe zu
}
\description{
jeder Voronoigrenzflaeche ihre U-hoehe = distanz der jeweiligen anliegenden Punke zuordnen
}
\usage{
Uheight4Voronoi(X, Y, Delaunay, PlotIt)
}
\arguments{
  \item{X}{ X Komponente der Punktkoordinaten}
  \item{Y}{ Y Komponente der Punktkoordinaten}
  \item{Delaunay}{Delaunaygraph. Wenn nicht gegeben: \code{DelaunayGraphMatrix(X,Y)} (aus GraphAlgorithms)}
  \item{PlotIt}{Wenn TRUE heisst zeichnen der Linien entlang der VoronoiDistanz. Standart: FALSE}
}

\value{\item{MiddlePoints}{}}