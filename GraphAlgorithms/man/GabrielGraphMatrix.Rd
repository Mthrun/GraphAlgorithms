\name{GabrielGraphMatrix}
\alias{GabrielGraphMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to calculate the Gabriel Graph.}
\description{
Algorithm to calculate the Adjacency matrix of the Gabriel Graph.
Two possibilities: 1. Heuristic rejection algorithm by Bhattacharya for Gabriel Graph Construction.
				   2. Usage of R-package spdep.
}
\usage{
GabrielGraphMatrix(X,Y,PlotIt=FALSE,Path=paste0(SubversionDirectory(),'PUB/dbt/GraphAlgorithms/src/'),Calcparallel=FALSE,inst=FALSE)
}
\arguments{
  \item{X}{Point coordinates, numeric vector[1:d].}
  \item{Y}{Point coordinates, numeric vector[1:d].}
  \item{PlotIt}{Optional: if TRUE, Gabriel graph is plotted, Boolean.}
  \item{Path}{Optional: Path where the Rcpp program GabrielGraph.cpp is saved.
			  Default set to Subversion. }
  \item{Calcparallel}{Optional: if TRUE -> parallel programming. Meaning more than one calculation at a time.
					  Note: does not work properly yet.}
  \item{Inst}{Optional: Short for instance.
						If FALSE, Bhattacharya's algorithm is used to calculate the Gabriel Graph (using Rcpp).
						If TRUE a function of the spdep Package is used. Note: Does not work properly for matrices containing Integers.}
}
\details{
Inst=FALSE:
Heuristic rejection algorithm for Gabriel Graph Construction Algorithm is ~ O(d n^2):
At the start of the algorithm all points are possibly connected. 
Next every point (X_i,Y_i) is checked to see if it is connected to any other points (X_j,Y_j) by the Gabriel Graph.
The requirement for the connecting edge of the points (X_i,Y_i) and (X_j,Y_j) to be part of the Gabriel Graph is defined as:
d((X_i,Y_i),(X_j,Y_j))2 <= d((X_i,Y_i),(X_k,Y_k))2 + d((X_j,Y_j),(X_k,Y_k))2,
d(x,y) being the euclidean distance.
If the condition is not fulfilled the edge between the points (X_i,Y_i) and (X_j,Y_j) gets deleted as a possible edge,
otherwise it is part of the Gabriel Graph.
Inst=TRUE:
The R-package spdep calculates the connection of the Gabriel Graph between points with the function gabrielneigh().
}

\value{
%- description of return values
	\item{Gabriel}{Adjacency matrix of the Gabriel Graph, numeric matrix[1:d,1:d].
				   If two points (X_i,Y_i) and (X_j,Y_j) are connected in the Gabriel Graph -> Gabriel[i,j]=Gabriel[j,i]==1.
				   Otherwise Gabriel(i,j)=0.}
	\item{Edges}{Matrix containing the indices of the adjacency matrix that equal 1, numeric matrix[1:n,1:2].
				 The points of the Gabriel Graph that are connected. 
				 If edges returns (i,j), points (X_i,Y_i) and (X_j,Y_j) are connected.}
}
\references{
see Bhattacharya, Application of computational geometry to pattern recognition problems, et al. 1981.
http://digitool.library.mcgill.ca/view/action/singleViewer.do?dvs=1423481178714~470&locale=en_US&show_metadata=false&VIEWER_URL=/view/action/singleViewer.do?&DELIVERY_RULE_ID=6&adjacency=N&application=DIGITOOL-3&frameId=1&usePid1=true&usePid2=true
	R-package spdep, 
	http://cran.r-project.org/web/packages/spdep/spdep.pdf .
}
\author{ Rabea Griese }

\examples{
X <- runif(100)*100
Y <- runif(100)*100
#GabrielGraphMatrix(X,Y,TRUE)
}