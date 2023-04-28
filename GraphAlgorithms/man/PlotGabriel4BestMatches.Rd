\name{PlotGabriel4BestMatches}
\alias{PlotGabriel4BestMatches}

\title{Plot Gabriel4BestMatches}
\description{
Zeichnen des Delaunay Graphen, Punkte ggf. nach Cls gefaerbt
}
\usage{
PlotGabriel4BestMatches(BestMatches,MatrixOrSize,Cls,IsTiled)
}
\arguments{
  \item{BestMatches}{BestMatches, (1:d,1:3)}
  \item{MatrixOrSize}{Default 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches, (2)}
  \item{Cls}{Classes of bestmatches  or []}
  \item{IsTiled}{==1 (default) => Randkorrigiertes Universum}
}
\details{
...
}
\value{
%- description of return values
	\item{Gabriel}{Sparse Vector des Gabrielgraphen in squareform, (1:d,1:d)}
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
}
\author{ Rabea Griese }

\keyword{Gabriel}%- zero or more keywords (all storaged in /doc/KEYWORDS, for search and help) 