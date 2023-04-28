HighdimensionalGabrielGraph=function(Data,  Genauigkeit= -0.000001){
# GabrielAdj=HighdimensionalGabrielGraph(Data)
# GabrielGraph im Hochdimensional mit ncol(Data)>2, nicht toroid
# Langsam, da O(n^3)
#INPUT
# Data[1:n,1,d] 		HichdimensionalData as a matrix. n is number of points, d number of variables
# Genauigkeit				int of EPS, for ball not empty
# OUTPUT
# adjascenc[1:n,1:n]  Gabriel AdjascencMatrix, 1= connected, Zero = Not connected Points
# inspired by ALUs matlab version
#author: MT 03/16
  n=nrow(Data)
  Distances=fastPdist(Data)^2
  #emptyball condition
  adjascenc=matrix(1,nrow = n,ncol = n)
  for(i in 1:n)
    for(j in 1:n){
      KathSumMinusHypoK = (Distances[i,]+ Distances[j,] - Distances[i,j])<Genauigkeit;
      if(sum(KathSumMinusHypoK)>0)# Ball Not empty
        adjascenc[i,j] = 0;
    }
  
  return(adjascenc)
}