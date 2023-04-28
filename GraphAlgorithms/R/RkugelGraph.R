RkugelGraph = function(DataOrDists,R,PlotIt=T,Points){
  # RKugelGraphAdjMatrix = RkugelGraph(DistanceMatrix,R)
  # Berechnung des Rkugel Graphen
  #  
  # INPUT
  # DataOrDists(1:n,1:n)              array of data: n cases in rows, d variables in columns, matrix is not symmetric
  #                           or distance matrix, in this case matrix has to be symmetric  
  # R                                     Kugelradius
  # OPTIONAL
  # PlotIt
  # Points[1:n, 1:d]      where d is2 or 3 coordinates of the vertices
  # 
  # OUTPUT
  # RKugelGraphAdjMatrix                   Rkugel Graphen
  # 
  # MT 06/2015
  ######################################################################################
  
  getRadius4Dichte=function(DataOrDists,Delaunay){
    # getRadius4Dichte(Data,Delaunay)
    # Inter-Clusterdistanc durch ABC-Analyse abschaetzen  
    # Falls Delaunay gegeben fuer abstrakte P-Matrix uber AUmatrix-Distanzen 
    # ansonsten uber euklidische Distanzen fuer P-Matrix
    #
    # INPUT
    # Data(1:n,1:d)           d-dimensionaler Datensatz bestehend aus n Punkten
    # Optional
    # Delaunay(1:n,1:n)       Delaunay Graph, and den >0 Kanten werden die Mittelpunkte der Ecken bestimmt und die
    #                         datendichte gemessen
    # Output
    # Radius4Dichte           der Radius mit dem die Dichte = anzahl Punkte in Hyperkugel gemessen werden soll
    # Author: Michael Thrun?
    
    
    # Note
    # Allgemein gilt
    # Die P-Matrix abbildung sollte glatt sein, wenn sie zu huppelig ist, ist der Radius zu klein
    # P-Matrix muss dort Senken haben, wo U-Matrix Hoehen hat
    # Wenn P-Matrix Abbildung zu wenige details hat, ist sie zu glatt und somit der Radius zu hoch
    if(isSymmetric(unname(DataOrDists))){
      x = DataOrDists
    }else{ #!isSymmetric
      #DataDists=as.matrix(dist(DataOrDists,method = "euclidean",diag=TRUE))
      if(isTRUE(requireNamespace("parallelDist"))){
        x = as.matrix(parallelDist::parallelDist(DataOrDists))
      }else{
        x = as.matrix(stats::dist(DataOrDists))
      }
    }# end if(isSymmetric(DataOrDists))
    
    
    
    if(missing(Delaunay)){ 
      
      # x=as.matrix(dist(Data))
      # pareto=ParetoRadius(Data)
      x=x[lower.tri(x, diag = FALSE)]
      par=quantile(x,c(0.2013)) #geschaetzter paretorRadius
      xx=ABCRemoveSmallYields(x,0.5)
      x=xx$SubstantialData
      res=suppressWarnings(ABCanalysis(x))
      Radius=min(x[res$Aind])/max(x[res$Cind])#*par  #Verhaeltnis vermutliche inner/Inter Clusterdistanz
      #print(min(x[res$Aind])/max(x[res$Cind]))
      # print(par)
      #  print(Radius)
      #  print(paste0('Geschaetzter Radius der P-Matrix ist ',round(Radius4Dichte,2)))
      
    }else{
      AUmatrix = Delaunay*x#
      y=AUmatrix[lower.tri(AUmatrix, diag = FALSE)]
      #Auhoehe=AUmatrix
      Auhoehe=y[y>0]
      res=ABCanalysis(Auhoehe)
      #  Radius4Dichte=(min(Auhoehe[res$Aind])
      #	Radius4Dichte=sqrt(min(Auhoehe[res$Aind]))
      #Radius4Dichte=sqrt(1/2*(mean(Auhoehe[res$Aind])+min(Auhoehe[res$Aind])))
      Radius4Dichte=min(Auhoehe[res$Aind])/max(Auhoehe[res$Cind]) #Verhaeltnis Inter-zu InnerClusterDistanzen
      print(paste0('Geschaetzter Radius4dichte der abstrakten P-Matrix ist ',round(Radius4Dichte,2)))
      
    }
    #return(cbind(Radius,par,pareto))
    return(Radius)
  }
  
  #####################################################################################
  if(isSymmetric(unname(DataOrDists))){
    DistanceMatrix = DataOrDists
  }else{ #!isSymmetric
    if(isTRUE(requireNamespace("parallelDist"))){
      DistanceMatrix = as.matrix(parallelDist::parallelDist(DataOrDists))
    }else{
      DistanceMatrix = as.matrix(stats::dist(DataOrDists))
    }
  }# end if(isSymmetric(DataOrDists))
  
  if(missing(R)){
    R=getRadius4Dichte(DistanceMatrix)
  }

  #[AnzPunkte,N]= size(DistanceMatrix);
  AnzPunkte=nrow(DistanceMatrix)
  N=ncol(DistanceMatrix)
  
  RKugelGraphAdjMatrix=matrix(0,ncol=N,nrow=N)
  
  for(i in 1:AnzPunkte){
    RInd = which(DistanceMatrix[i,] <= R,arr.ind=T)
    RKugelGraphAdjMatrix[i,RInd]=1
  }# for i=1:AnzPunkte
  
  if (PlotIt&!missing(Points)) { # Plot der gesamten Adjazenzmatrix mit punkten
    #figure();
    gplot(RKugelGraphAdjMatrix,Points); # Plot der gesamten Adjazenzmatrix mit doppelten punkten  
  }# end if 
  
  return(RKugelGraphAdjMatrix)
}