#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double numerator(DataFrame newData, NumericVector a) {
  NumericVector dat = sum(newData["thisChoos"]);  
  NumericVector a = a;
  NumericVector num = (a-1)+ dat;
  return(num)
  }


double denominator(DataFrame newData, NumericVector b, NumericVector lambdax) {
  NumericVector dat = (newData["Lambda"]);  
  NumericVector lambdax =lambdax;
  NumericVector b = b;
  NumericVector dem = b+sum(1/(lambdax+dat));
  return(dem)
}

//

/*** R
Lambda<-c(2,3,4,5,2)
thisChos<-c(1,1,1,0,0)
newData<-cbind(Lambda,thisChos)
newData<-as.data.frame(newData)
library(Rcpp)
setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/")
sourceCpp('twolines.cpp')
*/
