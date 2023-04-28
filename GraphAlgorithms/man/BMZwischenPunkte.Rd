\name{BMZwischenPunkte}
\alias{BMZwischenPunkte}
\title{ Calculating the middle points of Delaunay-Lines}
\arguments{
\item{Delaunay}{Adjacency matrix of the Delaunay-Graph}
\item{BestMatches}{n by 2 Matrix containing the x and y coordinates of the n bestmatches in rows}
\item{Data}{ The n by m Matrix containing the n points of Data from which the bestmatches are calculated. BestMatch[n,] contains the bestmatch of Data[n,]}
\item{DataDist}{ The pairwise distances of all rows in Data as a full matrix}
}
\value{BMab: The coordinates of the points on the Delaunay-Line between points a and b}
\description{
 Calculating the middle points of Delaunay-Lines}

