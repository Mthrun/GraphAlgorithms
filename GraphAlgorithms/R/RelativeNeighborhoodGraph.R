`RelativeNeighborhoodGraph` <-
function(X,Y,Delaunay,Open=TRUE,r=1,Silent=TRUE){

# function [RNGraph,Gabriel,Delaunay] = RelativeNeighborhoodGraph(X,Y,Delaunay,Gabriel);
# % [RNGraph,Gabriel,Delaunay] = RelativeNeighborhoodGraph(X,Y,Delaunay,Gabriel);
# % Berechnung des Relative Neighborhood Graph RNG als Teilmenge des Gabriel Graphen GG
# %  
# % INPUT
# % [X(1:d),Y(1:d]                      Punktkoordinaten
# %
# % Delaunay(1:d,1:d)                   Adjazenzmatrix des Delaunay Graphen
# OPTIONAL
# Open	          If TRUE, open balls are used in the definition.
# r	a multiplier to grow the balls.
# % OUTPUT
# % RNGraph(1:d,1:d)                    Adjazenzmatrix des Relative Neighborhood Graphen
# 

#mct 2023
  #kuerze aus delaunay
  Distances = as.matrix(dist(cbind(X, Y)))
  del <- deldir::deldir(X, Y)
  n = nrow(Distances)
  RNG_adjascencyMat <- matrix(0, nrow = n, ncol = n)
  #ToDo anstatt del dir package, deldir adjancency nehmen,
  
  #also diesen code ruechwaerts
  # FromInd          = PointsAndIndices$ind1;    #  indices der Ausgangspunkte des Delanays
  # ToInd            = PointsAndIndices$ind2     #  indices der Endpunkte des Delanays
  # 
  # # Adjazenzmatrix befuellen
  # UniqDelaunay = zeros(length(UniqX),length(UniqY)); # Adjazenzmatrix initialisieren
  # for (i in 1:length(FromInd)) {
  #   UniqDelaunay[FromInd[i],  ToInd[i]]  <- 1 ;#Only Direct neighbours A and B get an one from A to B
  #   UniqDelaunay[ToInd[i]  ,FromInd[i]]  <- 1 ;#Only Direct neighbours A and B get an one from A to B
  # } # end for i neighbours
  # ende code ruechwaerts
  #shbortcut: wende delaunay erneut an
  if(isFALSE(Silent))
  message("RelativeNeighborhoodGraph: Deldir ready. Computing RNG")
  
  for (edge in 1:nrow(del$delsgs)) {
    # in del$delsgs stecken die indices des Delaunays von -> Nach 
    i <- del$delsgs$ind1[edge]
    j <- del$delsgs$ind2[edge]
    d <- min(apply(cbind(Distances[i,-c(i, j)], Distances[j,-c(i,j)]), 1, max))
    if (isTRUE(Open)) {
      if (r * Distances[i, j] < d) {
        RNG_adjascencyMat[i, j] <- 1
        RNG_adjascencyMat[j, i] <- 1
      }
    }
    else {
      if (r * Distances[i, j] <= d) {
        RNG_adjascencyMat[i, j] <- 1
        RNG_adjascencyMat[j, i] <- 1
      }
    }
  }
  return(RNG_adjascencyMat)
  # % Naive Schnittkreis, vermutlich nicht sehr effizient
# ALUS Code
# if (nargin < 3) | (length(Delaunay)==0) % Delaunay Matrix ausrechnen
#     Delaunay  = DelaunayGraphMatrix(X,Y,0);  
# end;
# 
# if (nargin < 4) % Gabriel Graph ausrechnen
#     Gabriel = GabrielGraph(X,Y,Delaunay);
# end;
# 
# AnzCases =length(X);
# [AInd BInd] = find((Gabriel.*triu(ones(AnzCases,AnzCases)))>0); % AB indices
# 
# AnzPunkte=length(AInd);
# RNGraph = Gabriel ;              % aus dem gabriel werden kanten geloescht -> RNG
# for i=1:AnzPunkte
#     A = [X(AInd(i)),Y(AInd(i))]; % Punkt A
#     B = [X(BInd(i)),Y(BInd(i))]; % Punkt B
#     Radius = sqrt((B-A)*(B-A)');
#     Dist2A =  dist2all(A,[X,Y])';
#     InKreisAInd = find(((Dist2A-Radius) < 0.0001));  % nur die im Umkreis von A
#     Dist2B =  dist2all(B,[X(InKreisAInd),Y(InKreisAInd)])';
#     IstInLune = find((Dist2B-Radius) < 0.0001) ;
#     AnzInLune = length(IstInLune)-2 ; % 
#     if AnzInLune>0  % Kante Loeschen
#         RNGraph(AInd(i),BInd(i)) =0;
#         RNGraph(BInd(i),AInd(i)) =0;
#     end; % if AnzInLune>0  % Kante Loeschen
# end;% for i=1:AnzPunkte
}

