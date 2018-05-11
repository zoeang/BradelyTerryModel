#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]

NumericVector postLambdaAllDocs(DataFrame lambda){
sapply(paste0(unique(lambda[0])), FUN=postLambdaX, lambda=lambda, listAllData= listAllDocs );
}