PlotGabrielGraph <- function(X,Y){

# Zeichnen des Gabriel Graphen
# INPUT
# [X(1:d),Y(1:d)]      Punktkoordinaten
# 
#
 
 
    Gabriel  <- GabrielGraphMatrix(X,Y,PlotIt=T)
    title('Gabriel Graph')
 #Delaunay <- GabrielGraphMatrix(X,Y)$Delaunay
 
 #if (PlotNotDGedges ==1)
 #   # NotDelaunayEdges  Graph der Kanten die nicht mehr im Delaunay sonder nur noch im Gabriel sind
 #   gplot(Delaunay,cbind(X,Y),col="blue"); 
 #   par(new = TRUE)
 #   gplot(Gabriel,cbind(X,Y),'b-');
 #else




 }

