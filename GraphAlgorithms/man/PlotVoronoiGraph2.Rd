\name{PlotVoronoiGraph}
\alias{PlotVoronoiGraph}
\title{Zeichnen der Voronoizellen}
\description{
  Zeichnen der Voronoizellen
}
\usage{
PlotVoronoiGraph(X,Y,Cls=X*0+1,OnlyRand=0,isToroid=1)
}
\arguments{
  \item{X, Y}{Punktkoordinaten}
  \item{Cls}{Optional: Klassen der Bestmatches}
  \item{OnlyRand}{Optional: Nur den Delaunay Graphen fuer Randpunkte von Clustern}
  \item{isToroid}{Default: 1 -> Randkorrigiertes Universum}
}