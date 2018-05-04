#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame subsetlambdas(DataFrame lambdas, IntegerVector DocIds) {
  NumericVector lambdavals = lambdas["V2"]; // get the IDs in the dataframe
  NumericVector DocJs = lambdas["DocId"]; // get the other vector(s) in x
  IntegerVector idx; // An integer vector to store the rows to keep
  int n = lambdavals.size(), m = DocIds.size(); // Just object size variables
  for ( int i = 0; i < n; ++i ) { // for every row in the dataframe
    for ( int j = 0; j < m; ++j ) { // for every id in ids
      if ( lambdavals[i] == DocIds[j] ) { // for every x_id in x_ids
        idx.push_back(i); // if x_id is in ids, add it to idx
      }
    }
  }
  // create a new dataframe out of each column of x,
  // subsetting each vector by idx
  return DataFrame::create(_["DocJs"] = DocJs[idx], _["V2"] = lambdavals[idx]);
}