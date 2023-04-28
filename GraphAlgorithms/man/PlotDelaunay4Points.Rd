\name{PlotDelaunay4Points}
\alias{PlotDelaunay4Points}
\title{Zeichnen des DelaunayGraphen}
\description{Zeichnen des DelaunayGraphen nur fuer Punkte X(Index),Y(Index)}
\usage{
PlotDelaunay4Points(X,Y,Cls=X*0+1,Index=1:length(X),isToroid=1,OnlyRand=0)
}
\arguments{
  \item{X, Y}{Punktkoordinaten}
  \item{Cls}{Optional: Klassifizierung der Punkte}
  \item{Index}{Optional: Indexmenge fuer  X(Index),Y(Index)}
  \item{OnlyRand}{Optional: Nur den Delaunay Graphen fuer Randpunkte von Clustern}
  \item{isToroid}{Default: 1 -> Toroides Universum}
}
