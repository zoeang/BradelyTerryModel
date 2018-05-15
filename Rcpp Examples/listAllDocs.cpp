#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List listAllDocs(DataFrame lambda){
  // library(plyr)
  // List lapply(unique(lambda[0]), doc.x.dataframe);
  NumericVector DocId = unique(as<NumericVector>(lambda[0]));
  int n = DocId.size();
  List result(n);
  for ( int i = 0; i < n; ++i ) {
    result[i] = foo();
  }
  return result;
}