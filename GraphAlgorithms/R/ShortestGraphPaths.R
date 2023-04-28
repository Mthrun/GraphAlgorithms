ShortestGraphPaths=function(AdjacencyMatrix,CostMatrix,StartInd,EndInd,verbose=F,ShowWaitbar=F){
# % sh=ShortestGraphPaths(AdjacencyMatrix,CostMatrix);
# %
# % INPUT
# % AdjacencyMatrix(1:n,1:n)          adjacency matrix: A(i,j) ==0 if i not connected to j, A(i,j) == 1 <=>connected 
# % OPTIONAL
# % CostMatrix(1:n,1:n)              CostMatrix(i,j)  are the costs for path (i-> j), ==AdjacencyMatrix if not given
# %
# % StartInd                         these nodes are used a start points default StartInd=(1:n)
# % EndInd                           these nodes are used a start points default EndInd  =(1:n)
# % ShowWaitbar                      ==1 means to show a waiting bar default ==1
# %
# % OUTPUT
# % Costs
# % Paths
# author: MT
# % uses Joseph Kirk's dijkstra function see below
# NOTA: Doku as in matlab..  
AdiColumnNR = ncol(AdjacencyMatrix)
AdiLineNR = nrow(AdjacencyMatrix)

if(missing(CostMatrix)){# Nur pruefen wenn CostMatrix angegeben
CostColumnNR = AdiColumnNR
CostLineNR = AdiLineNR
}else{CostLineNR=
CostColumnNR= ncol(CostMatrix)
CostLineNR = nrow(CostMatrix)
}
NotTheSame = (AdiColumnNR-CostColumnNR +AdiLineNR-CostLineNR)

if(NotTheSame !=0){
    stop('ShortestGraphPaths dimensions of AdjacencyMatrix, and CostMatrix are not the same!')
}else { 
  n=nrow(AdjacencyMatrix) #[n,dummy] = size(AdjacencyMatrix);
  AllInd = c(1:n)
  
  if(missing(CostMatrix)){
      kirk = KirkDijkstra(AdjacencyMatrix,AdjacencyMatrix,AllInd,AllInd,waitbar=ShowWaitbar,verbose=verbose)
  }else if(missing(StartInd)){
      if(verbose) print('Calculating KirkDijkstra for Udistances')
      kirk = KirkDijkstra(AdjacencyMatrix,CostMatrix,AllInd,AllInd,waitbar=ShowWaitbar,verbose=verbose)
  }else if(missing(EndInd)){
      kirk = KirkDijkstra(AdjacencyMatrix,CostMatrix,StartInd,AllInd,waitbar=ShowWaitbar,verbose=verbose)
  }else{  
      kirk = KirkDijkstra(AdjacencyMatrix,CostMatrix,StartInd,EndInd,waitbar=ShowWaitbar,verbose=verbose)
  }
  
  return(list(Costs=kirk$costs,Paths=kirk$paths))
}
}