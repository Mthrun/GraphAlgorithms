GabrielGraphMatrix <- function(X, Y, PlotIt = FALSE,
                               path = paste0(SubversionDirectory(),'PUB/dbt/GabrielGraph/src/'),
                               calcparallel = FALSE, inst = TRUE, fast = TRUE){
  #GabrielGraphMatrix(X,Y,PlotIt=T,calcparallel=F,inst=T)
  # 
  # Function to construct Gabriel graph from an array of points
  #  
  # INPUT
  # X(1:d)
  # Y(1:d)                     Punktkoordinaten
  # path                       Pfad der Rcpp-Datei GabrielGraph.cpp
  # 
  # Optional
  # PlotIt                     if TRUE Gabriel graph is plotted
  # calcparallel               Kantenberechnung parallel falls TRUE
  # inst                       Bestimmt die Art der Berechnung von Gabriel
  #                            ==1 -> Rcpp. ==2 -> package spdep.
  #
  # OUTPUT
  # Gabriel(1:d,1:d)           Adjacency matrix of the Gabriel Graph
  # edges (1:n,1:2)            Matrix containing the connected points forming the Gabriel Graph
  #
  # Uses
  # uniquePoints(X,Y)
  # gplot(Gabriel,cbind(X,Y),Xlabel,Ylabel)
  # Author RG, 01/15
  # 1.Editor: MT, 01/15
  # 2.Editor: FL, 02/16
  
  if(fast){
    graph <- FastGabrielGraph(X,Y,PlotIt)
    return(graph)
  }
  
  anz <- length(X)
  
  # Punkte unique machen
  Unique = UniquePoints(cbind(X,Y))
  UniqXY = Unique$Unique
  UniqueInd   = Unique$UniqueInd
  Uniq2DataInd = Unique$Uniq2DatapointsInd
  IsDuplicate = Unique$IsDuplicate
  UniqX =  UniqXY[,1];
  UniqY =  UniqXY[,2];
  
  if(isTRUE(inst)){
    # Heuristic rejection Algorithm for Gabriel Graph Construction (Bhattacharya et al. 1981)
    # Algorithm is ~ O(d n^2)
    # USES
    # Rcpp Datei GabrielGraph.cpp
    # packages Rcpp
          
    #if(!require(Rcpp)){ # ggf package Rcpp laden
    #  install.packages('Rcpp')
    #  library(Rcpp)
   # }else{
    #  library(Rcpp)
   # }# end if(!require(Rcpp))
    
    # Paralleles Programmieren funktioniert noch nicht
    #if(calcparallel){
    #  library(parallel)
    #  numWorkers=8 #min=8, max=48 bei 4-8gb ram und 4 prozessoren
    #  Workers=makeCluster(numWorkers,type="PSOCK")
    #}
    
    #Rcpp Funktionen muessen geladen werden
    #suppressWarnings(sourceCpp(paste0(path,"GabrielGraph.cpp")))
    
    ndim <- ncol(UniqXY)
    npts <- nrow(UniqXY)
    #edges als Listen anlegen, die spaeter gefuellt werden
    edges1 <- vector("list", npts - 1)
    edges2 <- vector("list", npts - 1)
    
  
    #if(calcparallel){
    #vertices=lappy(1:(npts-1),FUN=function(i)return((i+1):npts))
    
    #for( i in 1:(npts-1) ) {
      # Berechnung der Kanten, die nicht zum Graphen gehoeren
    #adj=parLapply(Workers,1:(npts-1),fun=function(i,UniqXY){
    #  vertices=(i+1):npts
    #  excluded <- excl(UniqXY,vertices,vector(),i);
    #  adj <- vertices[which(!match(vertices,excluded,nomatch=F)>0)]
    #},UniqXY)
                    
    #for( i in 1:(npts-1) ) {
    #  # Kanten werden in die Listen edges1/2 gefuellt
    #  if(length(adj[[i]]) > 0) {
    #    edges1[[i]] <- rep(i,length(adj))
    #    edges2[[i]] <- adj  
    #  }
    #}
  #}else{
    
    ############ ORIGINALE VERSION
    #adjliste <- lapply(1:(npts-1),FUN=function(i,UniqXY){
    #  vertices <- (i+1):npts
    #  excluded <- excl(UniqXY,vertices,vector(),i);
    #  adj      <- vertices[which(!match(vertices,excluded,nomatch=F)>0)]
    #},UniqXY)
    ############ ENDE ORIGINALE VERSION
    
    #### alternative
    #adjliste <- getAdjList(npts, UniqXY)
    ####
    
    
                    
  #  for( i in 1:(npts-1) ) {
      # Kanten werden in die Listen edges1/2 gefuellt
  #    adj=adjliste[[i]]
  #    if(length(adj) > 0) {
  #      edges1[[i]] <- rep(i,length(adj))
  #      edges2[[i]] <- adj  
  #    }
    #}
    
  #   for( i in 1:(npts-1) ) {
  #     # Moegliche Kanten fuer den Graphen
  #     vertices <- (i+1):npts
  #   # Berechnung der Kanten, die nicht zum Graphen gehoeren
  #   excluded <- excl(UniqXY,vertices,vector(),i);
  #   adj <- vertices[which(!match(vertices,excluded,nomatch=F)>0)]
  #   # Kanten werden in die Listen edges1/2 gefuellt
  #   if(length(adj) > 0) {
  #     edges1[[i]] <- rep(i,length(adj))
  #     edges2[[i]] <- adj  
  #   }
  #   
  #   }
  #}
    # Umwandlung der Edges in Vektoren
    #edges1 <- unlist(edges1)
    #edges2 <- unlist(edges2)
    
    res    <- getAdjList(npts, UniqXY)
    edges1 <- res[[1]]
    edges2 <- res[[2]]
    lenedg <- res[[3]]
    edges1=edges1[1:lenedg]
    edges2=edges2[1:lenedg]
    # Leere Matrix befuellen
    sUG <- length(UniqXY[,1])
    #lenedg <- length(edges1)
    UniqGabriel <- matrix(0,nrow=sUG,ncol=sUG);
    UniqGabriel = Gab(UniqGabriel,edges1,edges2);
    
    
    #jetzt uniqe points wieder auf originale uebertragen
    Gabriel = matrix(0,nrow=anz,ncol=anz);
    Gabriel = UniqGabriel[Uniq2DataInd,Uniq2DataInd]
    
    Part1 = Uniq2DataInd[IsDuplicate] # Connect the duplicates
    Part2 = which(IsDuplicate)
    Gabriel[Part1, Part2] = 1
    Gabriel[Part2, Part1] = 1
    
    
  if(calcparallel){
    print('Operator: Firing workers')
    stopCluster(workers)
    
  }
  }

  if(!isTRUE(inst)){
    #Berechnung basierend auf Paket spdep, gibt zu viele Kanten aus  
    #uses    package spdep     
    #        gplot(AdjacencyMatrix,Coordinates,Xlabel,Ylabel,xlim,ylim,LineSpec) 
    
    # if(!require(spdep)){ # ggf package spdep laden
     # Calculates the Gabriel Graph
    # install.packages('spdep')
    # library(spdep)
    # }else{
     #  library(spdep)
     #  }# end if(!require(spdep)
    
    # Gabriel ausrechnen mit spdep
    requireNamespace('spdep')
    GabNeighOutput = spdep::gabrielneigh(UniqXY,nnmult=10); #  # dadrin stecken die indices des Gabriels von -> Nach
    FromInd =  GabNeighOutput$from;    #  indices der Ausgangspunkte des Gabriels
    ToInd   =  GabNeighOutput$to     #  indices der Endpunkte des Gabriels
    
    # Adjazenzmatrix befuellen
    UniqGabriel = matrix(0,nrow=length(UniqX),ncol=length(UniqY)); # Adjazenzmatrix initialisieren
    for (i in c(1:length(FromInd))) {
     UniqGabriel[FromInd[i],  ToInd[i]]  <- 1 ;#Only Direct neighbours A and B get an one from A to B
     UniqGabriel[ToInd[i]  ,FromInd[i]]  <- 1 ;#Only Direct neighbours A and B get an one from A to B
    }
    # jetzt uniqe points wieder auf originale uebertragen
    Gabriel = matrix(0,nrow=length(X),ncol=length(Y));
    Gabriel = UniqGabriel[Uniq2DataInd,Uniq2DataInd]
    Gabriel = Gabriel + IsDuplicate # noch je eine Verbindung zwischen den Doubletten eintragen
  }

  edgesx <- which((Gabriel*upper.tri(ones(anz,anz)))>0,arr.ind=TRUE)
  edgesx <- edgesx[order(edgesx[,1]),]
  edges <- cbind(as.vector(edgesx[,1]),as.vector(edgesx[,2]))

  if (PlotIt) { # Plot der gesamten Adjazenzmatrix mit doppelten punkte
    figure();
    gplot(Gabriel,cbind(X,Y),Xlabel='x',Ylabel='Y'); # Plot der gesamten Matrix mit doppelten punkten  
    title('Gabriel Graph')
  }# end if 

  return(list(Gabriel=Gabriel,edges=edges))
}


# Weitere Moeglichkeit, jedoch sehr aufwendig
#
#GabrielGraphMatrix <- function(X, Y, Delaunay=DelaunayGraphMatrix(X,Y)$Delaunay, PlotIt=FALSE){
#
#  # Berechnung des Gabriel Graphen GG als Teilmenge des Delaunay Graphen DG
#  # Naive Umkreismethode, vermutlich nicht sehr effizient
#  #  
#  # INPUT
#  # [X(1:d),Y(1:d)]                     Punktkoordinaten
#  # 
#  # Optional
#  # Delaunay(1:d,1:d)                   Adjazenzmatrix des DelaunayGraph
#  # PlotIt                              if TRUE Gabriel graph is plotted
#  # 
#  # 
#  # OUTPUT
#  # Gabriel                             Adjazenzmatrix des Gabriel  Graphen
#  # Delaunay                            Adjazenzmatrix des DelaunayGraph
#  
#
#AnzCases <- length(X);
## Delaunay ggf kuerzen
#Delaunay <- Delaunay[1:AnzCases,1:AnzCases];
#AInd  <- which((Delaunay*upper.tri(ones(AnzCases,AnzCases)))>0,arr.ind=TRUE)[,1];
#BInd <- which((Delaunay*upper.tri(ones(AnzCases,AnzCases)))>0,arr.ind=TRUE)[,2]; # AB indices
#
#
#AnzPunkte <- length(AInd);
#Gabriel   <- Delaunay;              # aus dem Delaunay werden kanten geloescht -> Gabriel
#Radius <- numeric(AnzCases);
#for (i in 1:AnzPunkte){
#  A <- cbind(X[AInd[i]],Y[AInd[i]]); # Punkt A
#  B <- cbind(X[BInd[i]],Y[BInd[i]]); # Punkt B
#  M <- 0.5 *(A+B);                   # Mitte zwischen A und B
#  Radius[] <- 0.5*sqrt((B-A)%*%t(B-A));
#  Dist2M <-  t(dist2All(M,cbind(X,Y)));
#  IstInKugel <- which((Dist2M-Radius) < 0.0001,arr.ind=TRUE);
#  AnzInKugel <- size(IstInKugel)[1]-2 ;
#  if (AnzInKugel>0){  # Kante Loeschen
#    Gabriel[AInd[i],BInd[i]] = 0;
#    Gabriel[BInd[i],AInd[i]] = 0;
#  }
#}
#
#if (PlotIt) { # Plot der gesamten Adjazenzmatrix mit doppelten punkte
#  gplot(Gabriel,cbind(X,Y)); # Plot der gesamten Adjazenzmatrix mit doppelten punkten  
#}#
#
#return(list(Gabriel=Gabriel,Delaunay=Delaunay))
#}