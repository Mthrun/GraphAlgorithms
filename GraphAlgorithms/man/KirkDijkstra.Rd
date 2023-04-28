\name{KirkDijkstra}
\alias{KirkDijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
KirkDijkstra
}
\description{
The Dijkstra's algorithm to find the shortest paths and their costs
}
\usage{
KirkDijkstra(AorV, xyCorE, SID = NULL, FID = NULL, verbose = F, waitbar = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AorV}{
AorV A or V with A NxN adjacency matrix , with A (I , J ) ! = 0 if and only if I and J are connected V Nx2 ( or NX3 ) matrix with x , y , ( z) coordinates 
}
  \item{xyCorE}{
 xy or C or E xy NX2 ( or NX3 ) matrix with x , y, (z) coordinates. Only if A first argument is C NxN cost matrix , C (I , J ) contains the cost of moving from I to JE Px2 matrix containing a list of edges in the first two columns E3Px3 matrix a Lister the edge in the first two columns and their weightings in the third column contains
}
  \item{SID}{
1xL vector of starting points . If not specified, minimum paths for all N points to the target points calculated 
}
  \item{FID}{
1xL vector of starting points . If not specified, minimum paths for all N points to the target points calculated 
}
  \item{verbose}{
verbose If TRUE , status messages are output 
}
  \item{waitbar}{
waitbar Fals TRUE displays a loading bar , possible only under Windows ; only verbose = truee
}
}
\value{
LxM matrix of minimal cost of minimal paths
}
\references{
\url{https://www.mathworks.com/matlabcentral/fileexchange/12850-dijkstra-s-shortest-path-algorithm}
}
\author{
Florian Lerch, Michael Thrun
}
\note{
reimplementation of Joseph Kirk from Matlab
}

\keyword{KirkDijkstra}% use one of  RShowDoc("KEYWORDS")
\keyword{Dijkstra}% __ONLY ONE__ keyword per line
