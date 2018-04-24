#include<Rcpp.h>

using namespace Rcpp;
DataFrame get_list_elem(List x, int idx) {
  DataFrame tmp = as<DataFrame>(x[idx]);
  return tmp;
}
// [[Rcpp::export]]
double bradleyterryeasy(int a, int b, int id, List lambda, List datset ){
  NumericVector sumvec; // create empty vectors to store our sum elements
  NumericVector lambdavec; //create empty vector to extract the lambda elements we want
  DataFrame datsetID=as<DataFrame>(datset[id]); //create dataframe for subsetting in loop 
  DataFrame lambdaID=as<DataFrame>(lambda[id]); //create dataframe for subsetting in loop 
  NumericVector datDocj= datsetID[1];//vector of DocIDj corresponding to DocIDi designated by id
  for(int i=0; i<datsetID.nrow(); ++i){ //i needs to be length of test1[[4990]][,2]; solved?
    //NumericVector lambdaj=datset[[id]][,2] //vector of DocIDj for corresponding DocIDi comparison
    double sumunit=(1/(lambdaID[1]+lambda[datDocj[i]][1]));// 1/ lambdaI+ lambdaJ
    NumericVector sumvec= c(sumvec, sumunit); //vector of 1/lambdaI+lambdaJ for each comparison of DocIDI to DocIDJ
    }
  NumericVector summationterm = sum(sumvec);
  NumericVector output = (a-1+sum(datsetID[2])/(b+summationterm);
  return(output)
  }

  
  
