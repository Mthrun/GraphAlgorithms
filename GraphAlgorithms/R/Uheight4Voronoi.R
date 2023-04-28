Uheight4Voronoi <- function(X,Y,Delaunay = DelaunayGraphMatrix(X, Y)$Delaunay,PlotIt = FALSE){

numberofcases <- length(x);
delaunaydists <- DistanceMatrix(cbind(X,Y), method = "euclidean") * Delaunay;
aindbind <- which(upper.tri(Delaunay, F) * Delaunay > 0, arr.ind = T);
numberofcornerpoints <- nrow(aindbind);
MiddlePoints <- matrix(0, nrow = numberofcornerpoints, ncol = 2);
borderpoints1 <- matrix(0, numberofcornerpoints, 2);
borderpoints2 <- matrix(0, numberofcornerpoints, 2);
borderuheight <- vector(0, numberofcornerpoints);

	for(i in 1:numberofcases){
	a <- c(X[aindbind[i,1]],Y[aindbind[i,1]])
	b <- c(X[aindbind[i,2]], Y[aindbind[i,2]])
	borderuheight[i] <- delaunaydists[aindbind[i,1], aindbind[i,2]];
	}

return(list(MiddlePoints = MiddlePoints))
}
# [VoronoiVertices,VoronoiCells ] = VoronoiCellsAndVertices([X,Y]);% alle Voronoi Zellen ausrechnen
# if PlotIt ==1
#     plot3(X,Y,zeros(AnzCases,1),'b.');
# end;
# for i=1:AnzGrenzpunkte
#     Voronoi4A = VoronoiCells(AInd(i));                        % Cell der voronoicelle A                        
#     VoronoiPunkteAInd =  cell2num(Voronoi4A )';            % Punktindices der Voronoizelle A
#     VoronoiPunkteA = VoronoiVertices(VoronoiPunkteAInd,:); % Eckpunkte der Voronoizelle A
# 
#     Voronoi4B = VoronoiCells(BInd(i));                        % Cell der voronoicelle A                        
#     VoronoiPunkteBInd =  cell2num(Voronoi4B )';            % Punktindices der Voronoizelle B
#     VoronoiPunkteB = VoronoiVertices(VoronoiPunkteBInd,:); % Eckpunkte der Voronoizelle B
#     ABGrenzPunkte = intersect(VoronoiPunkteA,VoronoiPunkteB,'rows');  % Schnitt der beiden Voronoizellen
#     [AnzInGrenzlinie,dummy] = size(ABGrenzPunkte);
#     if AnzInGrenzlinie==2 % Grenzlinie
#       GrenzLiniePkt1(i,:) =  ABGrenzPunkte(1,:);
#       GrenzLiniePkt2(i,:) =  ABGrenzPunkte(2,:);
#     else % es gibt nur 1 Grenzpunkt
#       MzuGrenzVektoren = [(ABGrenzPunkte- M);(ABGrenzPunkte- M)]; % beide Vektoren identisch
#       GrenzLiniePkt1(i,:) =  ABGrenzPunkte(1,:);
#       GrenzLiniePkt2(i,:) =  ABGrenzPunkte(1,:);
#     end;% if AnzInGrenzlinie==2 % Grenzlinie
#     if PlotIt ==1
#            Line = [A;B];
#            hold on; plot3(Line(:,1),Line(:,2),[0,0]','b.','LineWidth',2);   
#            hold on; plot3(ABGrenzPunkte(:,1),ABGrenzPunkte(:,2),ones(AnzInGrenzlinie,1)*GrenzLinieUhoehe(i),'r-','LineWidth',2);   
#            hold off;
#      end; % PlotIt ==1 %
# end;% for i=1:AnzGrenzpunkte


# function [Gabriel,AInd,BInd,GPind,GrenzLiniePkt1,GrenzLiniePkt2,MittelPunkte]= Uheight4Voronoi(X,Y,Delaunay,PlotIt );
# % jeder Voronoigrenzflaeche ihre U-hoehe = distanz der jeweiligen anliegenden Punke zuordnen
# % Die Voronoikanten bestimmen, die zwischen 2 verschiedenen KlassenLiegen 
# 
# % INPUT
# % [X(1:d),Y(1:d]                     Punktkoordinaten
# % Delaunay(1:d,1:d)                  Delaunaygraph, wenn nicht gegeben oder == []:
# % PlotIt                             ==1 heisst zeichnen der Linien entlang der VoronoiDistanz
# % OUTPUT
# % GabrielDists(1:d,1:d)              die Euclid Distanzen im GabrielGraph
# % [AInd BInd](1:AnzGrenzpunkte)      indices der Punkte AB  A = [X(AInd(i)),Y(AInd(i))]; B analog
# % GPind(1:AnzGrenzpunkte)            GPind(i)==3 Delonay Linie geht durch GrenzLinie = teil des GG
# %                                    GPind(i)==1 => Delonay Linie geht nicht durch GrenzLinie; nicht teil des GG
# % GrenzLiniePkt1(1:AnzGrenzpunkte)   Endpunkt der Grenzlinie = Beruehrung der Voronoizelle
# % GrenzLiniePkt2(1:AnzGrenzpunkte)   Endpunkt der Grenzlinie = Beruehrung der Voronoizelle
	
