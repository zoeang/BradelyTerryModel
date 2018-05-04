#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double getlambda(DataFrame lambdas, IntegerVector DocIds){
  
  //for( int i=0; i<DocIds.size(); ++i){
  double x=DocIds[0];//
  NumericVector DocJs = lambdas["DocIDj"];//
  LogicalVector lambdamatch=DocJs==DocIds[x];
  Rcout << lambdamatch;
  DataFrame lambdaRow=lambdas[lambdamatch==1];
  Rcout << 2;
  double prior=lambdaRow["Lambda"];
  Rcout << 3;
//}
  return(prior);
}