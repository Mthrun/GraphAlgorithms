\name{ShortestGraphPaths}
\alias{ShortestGraphPaths}
\usage{
ShortestGraphPaths(AdjacencyMatrix,CostMatrix,StartInd,EndInd,verbose=F,ShowWaitbar=F)
}
\title{ShortestGraphPaths}
\arguments{
\item{AdjacencyMatrix}{
(1:n,1:n)          adjacency matrix: A(i,j) ==0 if i not connected to j, A(i,j) == 1 <=>connected  
}
\item{CostMatrix}{
Optional, (i,j)  are the costs for path (i-> j), ==AdjacencyMatrix if not given 
}
\item{StartInd}{
Optional, these nodes are used a start points default StartInd=(1:n)
}
\item{EndInd}{
Optional, these nodes are used a start points default EndInd  =(1:n)
}
\item{verbose}{
Optional, ==1 means to show a waiting bar default ==1
}
\item{ShowWaitbar}{
Optional,==1 means to show a waiting bar default ==1
}
}
\value{List with
Costs 

Paths
}
\description{
 uses Joseph Kirk's dijkstra 
 }
\author{
Michael Thrun
}
