#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double posteriorlambda(DataFrame newData, double docXprior, int a=1, int b=1){
  double numerator = (a-1) + sum(as<NumericVector>(newData["thisChoos"]));
  double denominator = (b + sum(1 / (docXprior + as<NumericVector>(newData["Lambda"]))));
  //  Rcout << numerator;
  //  Rcout << denominator;
  //  Rcout << numerator/denominator;
  return numerator/denominator;
}

