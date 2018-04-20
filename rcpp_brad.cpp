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
DataFrame ex_fun1(DataFrame x, IntegerVector ids) {
  IntegerVector x_ids = x["DocIdi"];  // get the IDs in the dataframe
  NumericVector DocIdj = x["DocIdj"]; // get the other vector(s) in x
  NumericVector choose = x["choose"]; 
  IntegerVector idx; // An integer vector to store the rows to keep
  int n = x_ids.size(), m = ids.size();// Just object size variables
  for ( int i = 0; i < n; ++i ) { // for every row in the dataframe
    for ( int j = 0; j < m; ++j ) { // for every id in ids
      if ( x_ids[i] == ids[j] ) { // for every x_id in x_ids
        idx.push_back(i); // if x_id is in ids, add it to idx
      }
    }
  }
  // create a new dataframe out of each column of x,
  // subsetting each vector by idx
  return DataFrame::create(_["DocIdi"] = x_ids[idx], _["DocIdj"] = DocIdj[idx], _["choose"]= choose[idx]);
}


DataFrame ex_fun2(DataFrame x, IntegerVector ids) {
  IntegerVector x_ids = x["DocIdi"];  // get the IDs in the dataframe // get the other vector(s) in x
  NumericVector lambda = x["lambda"]; 
  IntegerVector idx; // An integer vector to store the rows to keep
  int n = x_ids.size(), m = ids.size();// Just object size variables
  for ( int i = 0; i < n; ++i ) { // for every row in the dataframe
    for ( int j = 0; j < m; ++j ) { // for every id in ids
      if ( x_ids[i] == ids[j] ) { // for every x_id in x_ids
        idx.push_back(i); // if x_id is in ids, add it to idx
      }
    }
  }
  // create a new dataframe out of each column of x,
  // subsetting each vector by idx
  return DataFrame::create(_["DocIdi"] = x_ids[idx], _["lambda"]= lambda[idx]);
}
////////////////////////////////////////////////////////////////////////////////////////
//this is brad
//calling r function in rcpp
NumericVector callFunction(NumericVector x, Function f) {
  NumericVector res = f(x);
  return res;
}
//////
double brad(int a,int b,int id, DataFrame lambda, DataFrame dataset){
  //DataFrame subsetdata = ex_fun1(dataset,id);
  //DataFrame newlambda = ex_fun2(lambda,id);
  DataFrame subsetdata = dataset;
  DataFrame newlambda = lambda;
  DataFrame sumvec = DataFrame::create( Named("",R_NilValue));
  DataFrame lambdavec = DataFrame::create( Named("",R_NilValue));
  //NumericVector n= subsetdata.nrow();
  NumericVector n= lambda.nrow();
  //for(int i=0; i<n; i++){
  //  lambdajsubset = ex_fun2[lambda["DocIDi"], subsetdata["DocIDi"][i]]; //Is this work? [i]
  //  NumericVector lambdavec=c(lambdavec,lambdajsubset["Lambda"]); //c() in R
  //}
  for(int i=0; i<n; i++){
    NumericVector sumunit=(1/(newlambda["Lambda"] + lambdavec[i]));
    NumericVector sumvec=c(sumvec,sumunit); // as.vector ? 
  }
  NumericVector summationterm=sum(sumvec); 
  NumericVector output=(a-1+sum(subsetdata["Choose"]))/(b+summationterm); 
    return(output);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
