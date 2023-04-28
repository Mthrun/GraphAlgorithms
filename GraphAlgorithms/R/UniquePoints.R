UniquePoints <- function(Datapoints,Eps) {
  # V <- UniquePoints(Datapoints)
  # return only the unique points in Datapoints
  #
  # INPUT
  # Datapoints[1:n,1:d]   [1:n,1:d] matrix of Datapoints points of dimension d
  #				                the points are in the  rows
  #
  # Eps                   Optional,scalarabove zero that defines minimum non-identical euclidean distance between two points
  # OUTPUT
  # a list V containing:
  # Unique                  [1:k,1:d]      the Datapoints points  without duplicate points
  # IsDuplicate             [1:n,1:n]      for i!=j IsDuplicate[i,j]== 1  if Datapoints[i,] == Datapoints[j,]    IsDuplicate[i,i]==0
  # UniqueInd               [1:k]		       an index vector 1:k such that Unique ==  Datapoints[UniqueInd,], it has k non-consecutive numbers or labels, each label defines a row number within Datapoints[1:n,1:d] of a unique data point
  # Uniq2DatapointsInd      [1:n] 	       an index vector 1:n. It has k unique index numbers representing the arbitrary labels. Each labels is mapped uniquely to a point in Unique. Logically in a way such that Datapoints ==  Unique[Uniq2DatapointsInd,] (will not work directly in R this way) 
  #
  #Description: Euclidean distance is computed and used within. Setting \code{Eps} to a very small number results in the identification of unique data points. Setting epsilon to a higher number results in the definition of mesh points within an d-dimensional R-ball grap
  #MT 06/2022

  if(missing(Eps)){                 # ab dieser Distanz zwischen 2 punkten sind diese identisch
    Eps =0.0000000001
  }
  if (inherits(Datapoints,"matrix")) {
    # If Datapoints is a vector.
    Datapoints <- as.matrix(Datapoints)
  }
  AnzPoints = nrow(Datapoints)               # soviele punkte in den Daten
  rownames(Datapoints) = c(1:AnzPoints)      # gib den Zeilen als namen ihren zeilennummer
  
  if(!requireNamespace('parallelDist')){
    warning("UniquePoints: package parallelDist is not installed, falling back to dist().")
    dists = as.matrix(dist(Datapoints))     # Distance with diagonal = 0.
  }else{
    dists = as.matrix(parallelDist::parDist(Datapoints, method = "euclidean"))
  }
  
  IsDuplicate=rep(FALSE,AnzPoints)
  diag(dists)=Inf                              # in der diagonalen nicht suchen
  dists[upper.tri(dists)]=Inf                  # nur in eine richtung suchen reicht, sonst werden beide punkte entfernt

  #zeile ist die dublette
  #spalte ist der datenpunkt auf den die dublette verweist
  #kann moeglicherweise wieder eine dublette sein
  ind_all = which(dists < Eps, arr.ind = TRUE) # Get indices of duplicates.
  
  if(length(ind_all) == 0){    # keine duplikate gefunden
    return(list("Unique" = Datapoints,
                "UniqueInd" = c(1:AnzPoints), #sortind
                "Uniq2DatapointsInd" = c(1:AnzPoints),
                IsDuplicate = IsDuplicate))
  }
  
  Unique    = unique(Datapoints)
  UniqueInd = as.numeric(row.names(Unique))
  
  tmpVar                = as.character(unique(ind_all[, 1])) # Get unique points with close points nearby - "search from one direction"
  ind                   = ind_all[tmpVar, ,drop=FALSE]       # Keep first occurancy, drop rest
  IsDuplicate[ind[, 1]] = TRUE                               # Mark duplicates as such
  uniqueDatapoints      = Datapoints[IsDuplicate==FALSE, ,drop=FALSE] # MT: Korrektur, falls genau eine Dopplung besteht
  
  Uniq2DatapointsInd               = rep(0, dim(Datapoints)[1])
  Uniq2DatapointsInd[!IsDuplicate] = 1:dim(Unique)[1]
  
  MyDuplicates = which(IsDuplicate)
  MyAssignment = c()
  for(i in 1:length(MyDuplicates)){
    myDup = Datapoints[MyDuplicates[i], ]
    for(j in 1:dim(Unique)[1]){
      if((Unique[j,1] == myDup[1]) & (Unique[j,2] == myDup[2])){
        MyAssignment = c(MyAssignment, j)
        break
      }
    }
  }
  
  if(length(MyAssignment) != length(MyDuplicates)){
    stop("There is an error assigning unique points to duplicates")
  }
  
  Uniq2DatapointsInd[MyDuplicates] = MyAssignment
  
  return(list("Unique"             = as.matrix(Unique),
              "UniqueInd"          = UniqueInd,
              "Uniq2DatapointsInd" = Uniq2DatapointsInd,
              "IsDuplicate"        = IsDuplicate,
              "ind_all"            = ind_all))#nur fürs debugging, spaeter entfernen
}
