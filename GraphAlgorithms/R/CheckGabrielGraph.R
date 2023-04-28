CheckGabrielGraph <- function(X,Y,edges=GabrielGraphMatrix(X,Y)$edges){
  #
  # Function to test if calculated Gabriel Graph is correct.
  # Using empty ball condition.
  #
  #Input
  # X(1:d)
  # Y(1:d)                     Punktkoordinaten
  #
  #Optional
  # edges (1:n,1:2)            Matrix containing the connected points forming the Gabriel Graph
  #
  #Output
  # emptyball                  logical, TRUE if GG is correct
  #
  # Author RG, 01/15
  #
  
  x <- cbind(X,Y)
  FromInd <- edges[,1]
  ToInd   <- edges[,2]
  Anz <- nrow(x)
  
  # Function to calculate euclidian distance between vectors
  # returns ||x-y||^(1/2)
  vecnorm2<- function(y)
    sum(y^2)^0.5
  
  
  emptyball=TRUE
  npts <- nrow(edges)
  
  for( i in 1:npts ) {
    
    #check if the empty ball condition isn't broked yet
    if(emptyball){
      #calculate distanz between two points supposedly connected by the GG.
      d1 <- vecnorm2( x[FromInd[i],] - x[ToInd[i],] )
      ball.empty <- TRUE
      
      #we want to check every point to see if there are any points 
      #within the circle around the points FromInd and ToInd.      
      for( k in (1:Anz)[!is.element(1:Anz, i)] ) {
        
        #exclude the points FromInd and ToInd (they don't have to be checked).
        if(x[FromInd[i],]!=x[k,] && x[ToInd[i],]!=x[k,]){
          
          #calculate distanz between FromInd to any other point k 
          #and ToInd to any other point k.
          d2 <- vecnorm2( x[FromInd[i],] - x[k,] )
          d3 <- vecnorm2( x[ToInd[i],] - x[k,] )
          #check to see if point k is in the circle.
          in.circle <- ( d1^2 > d2^2 + d3^2 )
          #if point k is in the circle, the ball is not empty
          ball.empty <- ball.empty && !in.circle
          #Only continue checking the Graph if the circle was empty
          if( in.circle ) 
            break
        }
      }
      #emptyball is FALSE, the empty ball condition has been violated
      if(!ball.empty ) {
        emptyball=FALSE
        break
      }
    }
  }
  return(emptyball=emptyball)
}

