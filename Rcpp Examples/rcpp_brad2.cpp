#include <Rcpp.h>
using namespace Rcpp;
//we subset data from R based on id
//Then, we can run Rcpp for bradleyterry only

double brad(int a,int b,int id, DataFrame lambda, DataFrame dataset){
  DataFrame subsetdata = dataset;
  DataFrame newlambda = lambda;
  IntegerVector lam = newlambda["Lambda"];
  DataFrame sumvec = R_NilValue;
  DataFrame lambdavec =R_NilValue;
  
  int n = newlambda.size();
  for( int i = 0; i < n; ++i ){
  NumericVector sums = sum(lam[i]);
  NumericVector sumunit = (1/ sums);
  }
  NumericVector sums = sum(lam);
  NumericVector sumunit = (1/ sums);
  NumericVector summationterm= sum(sumvec); 
  NumericVector output=(a-1+sum(subsetdata["Choose"]))/(b+summationterm); 
  return(output);
}


/*** R
brad(1,1,5499,Lambda,HIT2)
*/
