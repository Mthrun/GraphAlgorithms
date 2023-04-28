MinSpanTree <-
function(DataOrDistance,PlotIt=FALSE,Cls,Colors){
# MST=MinSpanTree(Data)
# Zeichnet einen 2 Dimensionalen Minimal spanning Tree
#
# INPUT
# DataOrDistance[d,d]   Der Datensatz oder die Distanzmatrix
# 
# Optional
# PlotIt
# Cls
# Colors
# OUTPUT
# TR                    an object of class spantree which is a list with two vectors, 
#                       each of length n-1. The number of links in a tree is one less 
#                       the number of observations, and the first item is omitted
# Depths                Depths of nodes
#
# Author: MT
# edited MT 2023
  
#requireNamespace('vegan')
#if(require(vegan)){library(vegan)}else{install.packages("vegan")
#library(vegan)}
requireNamespace('vegan')
if(isSymmetric(DataOrDistance)){
AllDists=DataOrDistance
}else{
AllDists =dist(DataOrDistance,method = "euclidean")
}
tr=vegan::spantree(AllDists);
if(isTRUE(PlotIt)){
  if (missing(Cls)) 
    Cls = rep(1, n)
  u = unique(Cls)
  uu = sort(u, decreasing = F)
  if (missing(Colors)) {
    mc = length(uu)
    if (mc > 1) 
      Colors = DataVisualizations::DefaultColorSequence[-2][1:mc]
    else Colors = "black"
  }
  ColorVec = Cls * 0
  k = 1
  for (i in uu) {
    ColorVec[Cls == i] = Colors[k]
    k = k + 1
  }
  
  if(isSymmetric(DataOrDistance)){
    plot(tr,  cmdscale(AllDists),col=ColorVec)
  }else if(dim(DataOrDistance)[2]>2){
    plot(tr,  cmdscale(AllDists),col=ColorVec)
  }
  else{
    plot(tr,DataOrDistance,col=ColorVec)
  }
}


return(list(TR=tr,Depths=vegan::spandepth(tr)))
}

