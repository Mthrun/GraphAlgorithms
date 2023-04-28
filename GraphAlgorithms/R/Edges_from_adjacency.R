Edges_from_adjacency = function(Adjacency, ProjectedPoints){
  
  if(!is.numeric(ProjectedPoints) | !is.matrix(ProjectedPoints)){
    stop("ProjectedPoints must be numeric matrix.")
  }
  if(dim(ProjectedPoints)[2] != 2){
    stop("ProjectedPoints must be numeric matrix with two columns.")
  }
  if(!is.numeric(Adjacency) | !is.matrix(Adjacency)){
    stop("Adjacency must be numeric matrix.")
  }
  if(dim(Adjacency)[1] != dim(Adjacency)[2]){
    stop("Adjacency must be numeric symmetric matrix.")
  }
  DIM = dim(Adjacency)[1]
  Edges = rbind()
  for(i in 1:DIM){
    for(j in 1:DIM){
      if(Adjacency[i,j] == 1){
        X1 = ProjectedPoints[i,1]
        X2 = ProjectedPoints[i,2]
        Y1 = ProjectedPoints[j,1]
        Y2 = ProjectedPoints[j,2]
        Edges = rbind(Edges, c(X1, X2, Y1, Y2))
      }
    }
  }
  return(Edges)
}