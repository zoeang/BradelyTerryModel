#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getlambda(DataFrame lambdas, IntegerVector DocIds){
  NumericVector DocJs = lambdas["DocIDj"];
  NumericVector Lambda = lambdas["Lambda"];
  int n = DocIds.size();
  NumericVector result(n);
  for( int i=0; i< n; ++i){
    double x = DocIds[i];
    LogicalVector lambdamatch = DocJs == x;
    NumericVector tmp = Lambda[lambdamatch==1];
    result[i] = tmp[0];
  }
  return result;
}