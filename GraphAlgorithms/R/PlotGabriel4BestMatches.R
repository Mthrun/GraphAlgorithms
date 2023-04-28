PlotGabriel4BestMatches <- function(BestMatches,MatrixOrSize=c(50,82),Cls=BestMatches[,1]*0+1,IsToroid=1){
  
  # Zeichnen des Delaunay Graphen, Punkte ggf. nach Cls gefaerbt
  # INPUT
  # BestMatches(1:d,1:3)        Bestmatches
  # path                            for the calculation of the GabrielGraph. path of GabrielGraph.cpp
  # 
  # OPTIONAL
  # MatrixOrSize[2]    Default 50,82; A vector of length 2, containing the number of 
  #                    lines and columns of the SOM corresponding to the BestMatches
  # Cls(1:d)           Classes of bestmatches  or []
  # IsTiled            ==1 (default) => Randkorrigiertes Universum
  #     
  # OUTPUT
  # Gabriel            Sparse Vector des Gabrielgraphen in squareform
  #
  # Uses
  # ClassPlot(X,Y,Cls,0,'.',0,xlim,ylim,PlotLegend)
  # gplot(Gabriel,cbind(X,Y),xlim,ylim)
  #
  # Autor: RG 11/2014
  #
  
  
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
  if (IsToroid  == 1){
    GBM <- Gabriel4BestMatches(BestMatches,MatrixOrSize=cbind(Lines, Columns),IsToroid=1)
    Gabriel <- GBM$Gabriel
    TiledGabriel <- GBM$TiledGabriel
    X <- GBM$X
    Y <- GBM$Y
    TiledCls <- c(Cls, Cls, Cls, Cls);
    # delaunay Graph zeichnen
    
    xlim <- c(min(X),max(X))
    ylim <- c(min(Y),max(Y))
    figure()
    plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE)    
    # weisse punkte sieht keiner 
    axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
    axis(2,ylim=ylim,col="black",las=1) #und y-Achse
    
    par(new = TRUE)
    ClassPlot(X,Y,TiledCls ,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
    par(new = TRUE)
    ClassPlot(X,Y,TiledCls ,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)
    par(new = TRUE) 
    gplot(TiledGabriel,cbind(X,Y),xlim=xlim,ylim=ylim)
    return(TiledGabriel)
  }
  
  else{
    Gabriel <- Gabriel4BestMatches(BestMatches,MatrixOrSize=cbind(Lines, Columns),IsToroid=0)$Gabriel
    X <- BestMatches[,3];
    Y <- Lines+1-BestMatches[,2]; 
    
    xlim <- c(min(X),max(X))
    ylim <- c(min(Y),max(Y))
    figure()
    plot(X,Y,col='white',xlim=xlim,ylim=ylim,axes=FALSE)  
    # weisse punkte sieht keiner 
    axis(1,xlim=xlim,col="black",las=1) #Festlegen der x-Achse
    axis(2,ylim=ylim,col="black",las=1) #und y-Achse
    
    # delaunay Graph zeichnen
    par(new = TRUE)
    ClassPlot(X,Y,Cls,0,'o',0,xlim=xlim,ylim=ylim,PlotLegend=0)
    par(new = TRUE)
    ClassPlot(X,Y,Cls,0,'.',0,xlim=xlim,ylim=ylim,PlotLegend=0)   
    par(new = TRUE)
    gplot(Gabriel,cbind(X,Y),xlim=xlim,ylim=ylim)
    return(Gabriel)
  }
}