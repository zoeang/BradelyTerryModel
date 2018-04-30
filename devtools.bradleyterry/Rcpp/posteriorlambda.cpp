#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double posteriorlambda(DataFrame newData, double docXprior, int a=1, int b=1){
  int numerator=(a-1)+R::sum(newData[0]);
  double denominator=(b+sum(1/(docXprior+newData[1])));
  return(numerator/denominator);
}