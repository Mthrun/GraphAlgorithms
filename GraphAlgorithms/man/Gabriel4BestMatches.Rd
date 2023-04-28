\name{Gabriel4BestMatches}
\alias{Gabriel4BestMatches}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
Calculates the adjacency matrix of the Gabriel graph for bestmatches in tiled form 
if BMs are located on a toroid grid}
\usage{
Gabriel4BestMatches(BestMatches, MatrixOrSize, IsToroid,PlotIt)
}
\arguments{
  \item{BestMatches}{BestMatches, (1:d,1:3)}
  \item{MatrixOrSize}{Default 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches, (2)}
  \item{IsToroid}{indicating if BM's are on a toroid grid. Default is True, Boolean}
  \item{PlotIt}{Set PlotIt=TRUE, if you want to see the Plots, Boolean}
}
\details{
%- typical short usage of functions
Typical usages are
  \preformatted{fun(arg1,arg2)}
  
  \code{fun name} long description
}
\value{
%- description of return values
	\item{Gabriel}{Sparse Vector des Gabrielgraphen in squareform, (1:d,1:d)}
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
}
\author{ Rabea Griese }


\keyword{ }%- zero or more keywords (all storaged in /doc/KEYWORDS, for search and help) 