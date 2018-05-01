#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double posteriorlambda(DataFrame newData, double docXprior, int a=1, int b=1){
  int numerator = (a-1) + sum(as<NumericVector>(newData[0]));
  double denominator = (b + sum(1 / (docXprior + as<NumericVector>(newData[2]))));
  return(numerator/denominator);
}