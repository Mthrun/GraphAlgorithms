Gabriel4BestMatches <- function(BestMatches,MatrixOrSize=c(50,82), IsToroid = TRUE,PlotIt=FALSE, fast=FALSE,XYcoords=F){
  # 
  # Calculates the adjacency matrix of the Gabriel graph for bestmatches in tiled form if BMs are located on a toroid grid
  #
  # INPUT
  # BestMatches[1:n,1:3]            n by 3 matrix containing the BMKey, X and Y coordinates of the n BestMatches
  #                                 BestMatches NEED NOT BE UNIQUE!
  #                                 however, there is an edge in the Deaunay between duplicate points!  
  #
  # OPTIONAL
  # MatrixOrSize[2]                 Default 50,82; A vector of length 2, containing the number of lines and columns of the SOM corresponding to the BestMatches
  # IsToroid                        logical, indicating if BM's are on a toroid grid. Default is True
  # PlotIt                          Set PlotIt=TRUE, if you want to see the Plots
  # XYcoords                        default: FALSE, if TRUE: XYKoordinaten werden nicht zu Lines,Columns umgerechnet
  # OUTPUT
  # a list of:
  # Gabriel[1:n,1:n]                adjacency matrix of the Gabriel-Graph
  # TiledGabriel[1:4n,1:4n]         if IsToroid,  this is the Tiled Gabriel Graph 
  # X, Y                            x- and y-coordinates of the tiled bestmatches (or just bestmatches if IsToroid = FALSE).
  #
  # Autor: RG 11/2014
  #
  # USES   TileBM(BestMatches)
  #        gplot(AdjacencyMatrix,Coordinates,Xlabel,Ylabel,xlim,ylim,LineSpec)
  
  if(is.vector(MatrixOrSize)&length(MatrixOrSize)==2){ # MatrixOrSize=c(..,..) 
    Lines  =MatrixOrSize[1]
    Columns=MatrixOrSize[2]
  }else{ # MatrixOrSize=0 Matrix
    Columns=ncol(MatrixOrSize)
    Lines  =nrow(MatrixOrSize)
  }# end(is.vector(MatrixOrSize)&length(MatrixOrSize)==2)
  # jetzt gibts Lines und Columns
  if (is.list(BestMatches)) 
    stop("BestMatches is a list not a matrix")
  if (ncol(BestMatches) > 3) 
    stop("BestMatches have wrong number of dimensions")
  if (ncol(BestMatches) < 1) 
    stop("BestMatches have wrong number of dimensions")
  if (ncol(BestMatches) == 2) {
    BestMatches = cbind(c(1:nrow(BestMatches)),BestMatches)
  }

  if (IsToroid) {
    if (max(BestMatches[, 2]) > Lines) 
      stop("max(BestMatches[, 2]) > Lines")
    if (max(BestMatches[, 3]) > Columns) 
      stop("max(BestMatches[, 3]) > Columns")
  }
  
  if(IsToroid){ # Behandlung eines pack-man (toroiden) MapSpaces
    TiledBestMatches=TileBM(BestMatches, Lines = Lines, Columns = Columns);
    if(XYcoords){
      TiledX=TiledBestMatches[,2]
      TiledY=TiledBestMatches[,3]
    }else{
      TiledX =TiledBestMatches[,3]; 
      TiledY = Lines*2+1-TiledBestMatches[,2]; # So rechnet man BM koordinaten in XY um
    }
    Tiled <- GabrielGraphMatrix(TiledX,TiledY, fast = fast);
    TiledGabriel <- Tiled$Gabriel
    # zurueckrechnen auf die ungekachelten werte
    #GabrielAdjazenzMatrix <-UntileGraph4BM(TiledGabriel, TiledBestMatches, Lines, Columns);
    Key = BestMatches[,1];  
    AnzBestMatches = length(Key);
    Offset = c(0,1,2,3)*AnzBestMatches
    GabrielAdjazenzMatrix = TiledGabriel[c(1:AnzBestMatches),c(1:AnzBestMatches)]; # initialisieren mit dem oberen viertel
    for (i in Offset){
      for(j in Offset){
        GabrielAdjazenzMatrix = GabrielAdjazenzMatrix + TiledGabriel[c(1:AnzBestMatches)+i,c(1:AnzBestMatches)+j];
      }# end for j
    }# end for i      
    X=TiledX #MT: Berechnung fehl!
    Y=TiledY
    GabrielAdjazenzMatrix =(GabrielAdjazenzMatrix>0)*1; # alles auf 0/1 reduzieren
    
  }else{# MapSpace ist planar
    if(XYcoords){
      X = BestMatches[,2];          #So rechnet man BM koordinaten in XY um
      Y = BestMatches[,3];  #So rechnet man BM koordinaten in XY um
    }else{
      X = BestMatches[,3];          #So rechnet man BM koordinaten in XY um
      Y = Lines+1-BestMatches[,2];  #So rechnet man BM koordinaten in XY um
    }
    GAM <- GabrielGraphMatrix(X,Y, fast = fast);
    GabrielAdjazenzMatrix <- GAM$Gabriel
    TiledGabriel <- GabrielAdjazenzMatrix;
  }# end if(IsToroid)
  
  if (PlotIt) { # Plot der gesamten Adjazenzmatrix mit doppelten punkte
    #figure();
    gplot(TiledGabriel,cbind(X,Y)); # Plot der gesamten Adjazenzmatrix mit doppelten punkten  
  }#
  
  # Ausgabe zusammenstellen
  return(list(Gabriel = GabrielAdjazenzMatrix, TiledGabriel = TiledGabriel, X =X, Y =Y))
}# end function Gabriel4BestMatches