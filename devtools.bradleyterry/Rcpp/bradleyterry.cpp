#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double bradleyterryeasy(int a, int b, int id, List lambda, List datset ){
  NumericVector sumvec; // create empty vectors to store our sum elements
  NumericVector lambdavec; //create empty vector to extract the lambda elements we want

  for(int i=0; i<datset[[id]].size; ++i){ //i needs to be length of test1[[4990]][,2]
    //NumericVector lambdaj=datset[[id]][,2] //vector of DocIDj for corresponding DocIDi comparison
    double sumunit=(1/(lambda[[id]][,2]+lambda[[datset[[id]][,2][i]]][,2]));// 1/ lambdaI+ lambdaJ
    NumericVector sumvec= c(sumvec, sumunit); //vector of 1/lambdaI+lambdaJ for each comparison of DocIDI to DocIDJ
    };
  NumericVector summationterm = sum(sumvec) ;
  NumericVector output = (a-1+sum(dat[[id]][,3])/(b+summationterm);
  return(output)
  }

  
  
