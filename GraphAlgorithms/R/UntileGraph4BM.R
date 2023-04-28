UntileGraph4BM <- function(TiledGraph, TiledBestMatches, Lines, Columns){
# UntiledGraph <-UntileGraph4BM(TiledGraph, TiledBestMatches, Lines, Columns)
# Given the adjacency matrix of a tiled graph and the tiled bestmatches, this returns the adjacency matrix of the untiled graph.
#Input:
# TiledGraph: The adjacency matrix of the tiled graph
# TiledBestMatches: The tiled BestMatches
# Lines, Columns: The number of lines and columns of the original SOM
#Output:
#UntiledGraph: the adjacency matrix of the untiled graph.

tiledbmx <- TiledBestMatches[,2];
tiledbmy <- TiledBestMatches[,3];
numberofbms <- (nrow(TiledBestMatches)/4);
onetonumberofbms <- c(1:numberofbms);
untiledind <- onetonumberofbms;
tiledind <- rep(untiledind, 4);
centralind <- which((tiledbmx > (0.4 * Lines)) & (tiledbmx <= (1.6 * Lines)) & (tiledbmy > (0.4 * Columns)) & (tiledbmy <= (1.6 * Columns)), arr.ind = T)
UntiledGraph <- 0 <(TiledGraph[onetonumberofbms, onetonumberofbms] + TiledGraph[onetonumberofbms + numberofbms, onetonumberofbms] + TiledGraph[onetonumberofbms, onetonumberofbms + numberofbms] + TiledGraph[onetonumberofbms + numberofbms, onetonumberofbms + numberofbms]);

for (i in 1:length(centralind)){
ci <- centralind[i];
connectedintiledind <- which(TiledGraph[i,] > 0, arr.ind = T);
connecteduntiledind <- tiledind[connectedintiledind];
untci <- tiledind[ci];
UntiledGraph[untci, connecteduntiledind] <- 1;
UntiledGraph[connecteduntiledind, untci] <- 1;
}

return(UntiledGraph = UntiledGraph)
}