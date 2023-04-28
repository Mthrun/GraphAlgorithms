Blaetter <- function(Adj){
# BlattInd <- Blaetter(Adj) ;
# die Indices der Blaetter eines Graphen
#
# INPUT
# Adj[1:n,1:n]           Adjazenzmatrix  passend zu den folgenden Knoten Infos
#
# OUTPUT
# BlattInd[1:b]          Indizes, so dass Adj[,BlattInd] sind die Bletter des Graphen

   BlattInd  <- which(rowSums(Adj) == 0)   ;  # die Indices der Blaetter
   return(BlattInd);
} # end function  Blaetter(Adj) ;
