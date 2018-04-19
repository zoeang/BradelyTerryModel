#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double bradleyterry(int a; int b, vec id; DataFrame lambda; DataFrame dataset ){
  DataFrame subsetdata=dataset[dataset['DocIDi'] %in% id,];
  DataFrame newlambda=lambda[lambda['DocId'] %in% id,];
  NumericVector sumvec=;
  NumericVector lambdavec=; 

  for(int i=0; i<subsetdata.nrow(); i++){
    DataFrame lambdajsubset= lambda[lambda['DocId'] %in% subsetdata['DocIDj'][i],];   /*** how to subset?    
    NumericVector lambdavec= c(lambdavec,lambdajsubset['Lambda']);
    };


  for (int i=0; i<nrow(subsetdata); i++){
   double sumunit = (1/(newlambda['Lambda']+lambdavec[i])); 
  NumericVector sumvec = as.vector(c(sumvec,sumunit)) ;
  }
  
  NumericVector summationterm = sum(sumvec) ;
 */
  DataFrame output = (a-1+sum(subsetdata['Choose'])/(b+summationterm);
  return(output)
  }