PlotVoronoi4BestMatches <- function (BestMatches,MatrixOrSize=c(50,82),Cls=BestMatches[,1]*0+1,OnlyRand=0,Tiling=1){
  
# Zeichnen der Voronoizellen mit Sites, ggf. durch Cls gefaerbt

# INPUT
# Bestmatches(1:d,1:3)        Bestmatches
# 
# OPTIONAL
# MatrixOrSize[2]     Default 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches
# Cls(1:d)            Classes of bestmatches  or []
# OnlyRand            Nur den Delaunay Graphen fuer Randpunkte von Clustern, default: ==0  
# Tiling              ==1 (default) Matrix wird  gekachelt
#     
# OUTPUT
# Delaunay            Sparse Vector des Delaunaygraphen in squareform
# TRI                 triangles as returned by delaunay(X,Y);

# USES  PlotVoronoiGraph(X,Y,Cls,OnlyRand,Tiling)
#       TileBM(BestMatches,Lines,Columns)


# Groesse der U-Matrix feststellen
if(is.vector(MatrixOrSize)&length(MatrixOrSize)==2){ # MatrixOrSize=c(..,..) 
  Lines   = MatrixOrSize[1]
  Columns = MatrixOrSize[2]
}else{ # MatrixOrSize=0 Matrix
  Columns = ncol(MatrixOrSize)
  Lines   = nrow(MatrixOrSize)
}# end(is.vector(MatrixOrSize)&length(MatrixOrSize)==2)
# jetzt gibts Lines und Columns

# ggf kacheln
if (Tiling  == 1)
BestMatches = TileBM(BestMatches,Lines,Columns);
Lines   = 2*Lines; 
Columns = 2*Columns;
Cls     = c(Cls, Cls, Cls, Cls);
# ggf tiling

X = BestMatches[,3];
Y = Lines+1-BestMatches[,2];
PlotVoronoiGraph(X,Y,Cls,OnlyRand,0);
return(list(X=X,Y=Y))
}