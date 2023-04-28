\name{CheckGabrielGraph}
\alias{CheckGabrielGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Check if the GabrielGraph is correct.}
\description{
Function to check if the Gabriel Graph, calculated by GabrielGraphMatrix.R, is correct using the empty ball condition.}
\usage{
CheckGabrielGraph(X,Y,Edges=GabrielGraphMatrix(X,Y)$Edges)
}
\arguments{
  \item{X}{The same point coordinates that were used to calculate the GabrielGraphMatrix, numeric vector[1:d].}
  \item{Y}{The same point coordinates that were used to calculate the GabrielGraphMatrix, numeric vector[1:d].}
  \item{Edges}{Optional: Matrix containing the edges of the GabrielGraph, calculated by 
			   GabrielGraphMatrix.R, numeric matrix[1:n,1:2]. 
			   If value is missing, X and Y are given to the function GabrielGraphMatrix.R
			   and the value will be calculated.}
}
\details{
This function uses the empty ball condition to see if the edges of the Gabriel Graph
are correct. 
Empty Ball condition: Points (X_i,Y_i) and (X_j,Y_j) are connected in the Gabriel Graph 
if no other point (X_k,Y_k) is inside the circle connecting the points.
}

\value{
%- description of return values
	\item{Emptyball}{Logical value, returns TRUE if the GabrielGraph is correct.}
}
\references{
see Bhattacharya, Application of computational geometry to pattern recognition problems, et al. 1981.
http://digitool.library.mcgill.ca/view/action/singleViewer.do?dvs=1423481178714~470&locale=en_US&show_metadata=false&VIEWER_URL=/view/action/singleViewer.do?&DELIVERY_RULE_ID=6&adjacency=N&application=DIGITOOL-3&frameId=1&usePid1=true&usePid2=true
}
\author{ Rabea Griese }

\examples{
X <- runif(100)*100
Y <- runif(100)*100
#edges <- GabrielGraphMatrix(X,Y,TRUE)$edges
#CheckGabrielGraph(X,Y,edges)
}
