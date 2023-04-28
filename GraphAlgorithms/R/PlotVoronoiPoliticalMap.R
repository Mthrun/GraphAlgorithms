PlotVoronoiPoliticalMap <- function(X,Y,Cls=X*0+1,BorderCorrection=1,showpoints=TRUE){
#
# Zeichnen der Voronoizellen eingefaerbt mit den ClsColors
# default mit Border correction, so dass keine offenen ungefaebten Randzellen zu sehen sind
# INPUT
# [X(1:d),Y(1:d)]      Punktkoordinaten
#
# OPTIONAL
# Cls(1:d)           Classes of bestmatches  or []
# BorderCorrection   ==1 (default) => Randkorrigiertes Universum
# showpoints         TRUE, Punkte werden angezeigt
#
# uses  CornerPoints(X,Y,Cls)
#       DefaultColors4Cls(Cls)
#       package deldir

#if(!require(deldir)){ # ggf package deldir laden
  # Calculates the Delaunay triangulation and the 
  # Voronoi tessellation (with respect to the entire plane) of a planar point set.
 # install.packages('deldir')
#  library(deldir)
#}else{
#  library(deldir)
#}# end if(!require(deldir))

up <- UniquePoints(cbind(X, Y)) # remove duplicates
CellColor = DefaultColors4Cls(Cls[up$sortind]); # remove the cls entries for the removed duplicates and calculate colors
if(BorderCorrection == 1){ # Eingaberaum kacheln
 CP <- CornerPoints(up$unique[,1], up$unique[,2],Cls[up$sortind])
  XPlusCorner <- CP$XPlusCorner;
  YPlusCorner <- CP$YPlusCorner;
  DelTriCor <- deldir(XPlusCorner,YPlusCorner)
  VoronoiCells <- tile.list(DelTriCor)
#  figure()
  plot(VoronoiCells,fillcol=CellColor,showpoints=showpoints,close=TRUE,xlim= xlim,ylim= ylim)
  title('Randkorrigierter Voronoi Graph')
}

else { # kein tiling: gewoehnlicher voronoi graph
  DelTri <- deldir(X[up$sortind],Y[up$sortind])
  VoronoiCells <- tile.list(DelTri)
  figure()
  plot(VoronoiCells,fillcol=CellColor,showpoints=showpoints,close=TRUE,xlim=xlim,ylim=ylim)
}
# Eingaberaum kacheln

}
