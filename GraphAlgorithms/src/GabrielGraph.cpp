//#include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double vecnorm(NumericVector x){
  // Function to calculate euclidian distance between vectors
  // ||x-y||^(1/2)
  //Input
  // x(1:n)         Vector of length n
  //Output
  // out            Norm of the Vector x
  //Author RG, 01/15
  
  double out;
  out = sqrt(sum(pow(x,2.0)));
  return out;
}

// [[Rcpp::export]]
NumericVector vektorzugriff(NumericMatrix  xy,int i){
  //Function get a row of a Matrix xy
  //Input
  // xy(1:n,1:m)    nxm-Matrix
  // i              Index of the row you want to get back
  //Output
  // out(1:m)       i-th row of the Matrix xy
  //Author RG, 01/15
  
  int col = xy.ncol();
  NumericVector out(col);
  for(int j=0; j<col; j++){
    out[j] = xy(i-1,j);
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector vergl(NumericVector x, NumericVector y){
  //Function to compare to Vectors, not necessarily of the same length
  //Input
  // x(1:n)         Vector of length n
  // y(1:m)         Vector of length m
  //Output
  // out(1:m)       i-th row of the Matrix xy
  //Author RG, 01/15
  
  IntegerVector out = match(x, y);
  return out;
}

// [[Rcpp::export]]
NumericVector toVec(NumericVector x, int i){
  //Function to add an Integer to the end of a Vector
  //Input
  // x(1:n)          Vector of length n
  // i               Integer to be added
  //Output
  // out(1:(n+1))    Vector x with i at the end 
  //Author RG, 01/15
  
  //turn i into Vector
  NumericVector dummy =  NumericVector::create( i );
  int len = x.size();
  int len2 = dummy.size();
  int k=0;
  NumericVector out(len+len2);
  //Fill new vector with x
  while(k<len+len2){
    if(k<len){
      out[k]=x[k];
      k++;
    }
    else{
      //add i to the Vector
      out[k]=dummy[k-len];
      k++;
    }
  }
  return out;
}


// [[Rcpp::export]]
LogicalVector isNA(IntegerVector x) {
  //Function to find NAs in a Vector
  //Input
  // x(1:n)          Vector of length n
  //Output
  // out(1:n)        logical Vector with TRUE if NA
  //Author RG, 01/15
  
  int n = x.size();
  LogicalVector out(n);  
  for (int i = 0; i < n; ++i) {
    out[i] = IntegerVector::is_na(x[i]);
  }
  return out;
}


// [[Rcpp::export]]
NumericVector excl(NumericMatrix  xy,NumericVector vertices,NumericVector excluded, int i){
  //Function to calculate the edges that are not in the Gabriel Graph
  //Input
  // xy(1:n,1:m)    nxm-Matrix
  // x(1:n)         Vector of all possible edges for the Graph
  // y(1:n)         Vector of edges that have already been eliminated as a poissibility
  // i              The number of the row of xy to be worked with
  //Output
  // excluded(1:n)  Vector of edges that are not in the Gabriel Graph
  //Author RG, 01/15
 
  int npts = xy.nrow()+1;
  double d1; 
  double d2;
  double d3;
  
  for(int r=i+1; r<npts; r++) {
    // Ueberspringe vertices in excluded
    if(!is_true(any(isNA(vergl(r,excluded))))){
      continue;}
    
    d1 = vecnorm(vektorzugriff(xy,i) - vektorzugriff(xy,r));
    
    for(int k=1; k<npts; k++) {
      if((k!=r) && (k!=i)){
        d2 = vecnorm(vektorzugriff(xy,i) - vektorzugriff(xy,k));
        d3 = vecnorm(vektorzugriff(xy,r) - vektorzugriff(xy,k));
        
        //Betrachte vertices, die noch nicht excluded sind
        if(!is_true(any(isNA(vergl(k,vertices[isNA(vergl(vertices,excluded))]))))){
          //Wenn d(x,z)^2 > d(x,y)^2+d(y,z)^2 -> Kante gehoert nicht zum GG
          if( pow(d2,2.0) > pow(d1,2.0) + pow(d3,2.0) ) {
            excluded = toVec(excluded,k);
          }
        }
        //Wenn d(x,y)^2 > d(x,z)^2+d(y,z)^2 -> Kante gehoert nicht zum GG
        if( pow(d1,2.0) > pow(d2,2.0) + pow(d3,2.0) ){
          excluded = toVec(excluded,r);
          break;
        }
      }
    }
  }
  return excluded;
}

// [[Rcpp::export]]
NumericMatrix Gab(NumericMatrix Gabriel, NumericVector edges1, NumericVector edges2){
  //Function to fill a Matrix with ones
  //Input
  // Gabriel(1:n,1:n)    nxn-Matrix to be filled at the points of (edges1,edges2)
  // edges1(1:m)         Vector of length m
  // edges2(1:m)         Vector of length m
  //Output
  // Gabriel             nxn-Matrix filled at the points of (edges1,edges2)
  //Author RG, 01/15
  
  int anz = edges1.size();
  for(int i=0; i<anz; i++) {
    Gabriel(edges1[i]-1, edges2[i]-1)  = 1 ; 
    Gabriel(edges2[i]-1, edges1[i]-1)  = 1 ; 
  }
  return Gabriel;
}


 



// [[Rcpp::export]]
NumericVector filterIndexVector(NumericVector indexVector, NumericVector excluded){
  // indexvector containts indices. Every index whithin indexVector that is also
  // whitin excluded gets filtered out
  
  int i;
  LogicalVector keepIndices(indexVector.length());
  for(i = 0; i < indexVector.length(); ++i){
    keepIndices[i] = any( excluded == indexVector[i]).is_false();
  }
  return(indexVector[keepIndices]);
}

// [[Rcpp::export]]
List getAdjList(int npts, NumericMatrix UniqXY){
  
  //Rcpp::List adjliste;
  NumericVector edges1(npts*npts);
  NumericVector edges2(npts*npts);
  int counter = 0;
  NumericVector excluded;
  NumericVector adj;
  for(int i=1; i<=(npts-1); i++){ //lapply(1:(npts-1))
    int length = npts-(i+1)+1; //(i+1):npts
    NumericVector vertices(length); // vertices <- (i+1):npts
    for(int j=0; j<length; j++){
      vertices[j] = j+i+1;
    }
    excluded = excl(UniqXY, vertices, NumericVector(), i);
    adj = filterIndexVector(vertices, excluded);
    //adjliste.push_back(adj);
    
    

//Kanten werden in die Listen edges1/2 gefuellt
//adj=adjliste[[i]]
      if(adj.length() > 0) {
        int j;
        for(j = 0; j < adj.length(); j++){
          edges1[counter] = i;
          edges2[counter] = adj[j];
          counter++;
          
        }
        //edges1.push_back(
        //rep( NumericVector(i) ,adj.length());//); 
        //edges2.push_back( adj );  
      }
  }
  
  return(Rcpp::List::create(edges1,edges2, counter));
  //return(adjliste);
  
  
  
  
//for( i in 1:(npts-1) ) {
//    # Kanten werden in die Listen edges1/2 gefuellt
//    adj=adjliste[[i]]
//    if(length(adj) > 0) {
//      edges1[[i]] <- rep(i,length(adj))
//      edges2[[i]] <- adj  
//    }
  
  return(Rcpp::List::create());
}


// [[Rcpp::export]]
NumericMatrix fastPdist2(NumericMatrix Ar, NumericMatrix Br) {
    int m = Ar.nrow(), 
        n = Br.nrow(),
        k = Ar.ncol();
    arma::mat A = arma::mat(Ar.begin(), m, k, false); 
    arma::mat B = arma::mat(Br.begin(), n, k, false); 
 
    arma::colvec An =  sum(square(A),1);
    arma::colvec Bn =  sum(square(B),1);
 
    arma::mat C = -2 * (A * B.t());
    C.each_col() += An;
    C.each_row() += Bn.t();
 
    return wrap(sqrt(C)); 
}

// [[Rcpp::export]]
LogicalMatrix BrokenGabrielEdgesX(NumericMatrix x, IntegerMatrix edges){
  int Anz = x.nrow();
  int npts = edges.nrow();
  LogicalMatrix brokenEdges = LogicalMatrix(Dimension(Anz,Anz));
  
  // pairwise distances for all points
  NumericMatrix distances = fastPdist2(x,x);
    
  IntegerMatrix::Column FromInd = edges.column(0);
  IntegerMatrix::Column ToInd = edges.column(1);
  
  int i;
  for(i = 0; i < npts; i++){
    double d1 = distances(FromInd[i]-1 , ToInd[i]-1);
    
    int k;
    for(k = 0; k < Anz; k++){
      double d2 = distances(FromInd[i]-1, k);
      double d3 = distances(ToInd[i]-1, k);
      
      if( (d1*d1) > (d2*d2 + d3*d3))
        brokenEdges(FromInd[i]-1, ToInd[i]-1) = true;
    }
  }
  return(brokenEdges);
}