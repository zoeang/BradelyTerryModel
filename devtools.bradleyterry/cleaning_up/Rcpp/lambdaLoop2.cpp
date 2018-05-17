#include<Rcpp.h>


using namespace Rcpp;


// [[Rcpp::export]]
double posteriorlambda(DataFrame newData, double docXprior, int a=1, int b=1){//Update the lambda of one document once
  double numerator = (a-1) + sum(as<NumericVector>(newData["Choose"]));
  double denominator = (b + sum(1 / (docXprior + as<NumericVector>(newData["Lambda"]))));
  return numerator/denominator;
}

// [[Rcpp::export]]
NumericVector lambdaLoop2(DataFrame hits, NumericVector DocIds, DataFrame Hit3, NumericVector extractLambda){ //update the lambdas of all documents once
  IntegerVector docHit= hits["DocIDi"]; //create vectors from the columns of the dataframe
  IntegerVector Choose = Hit3["Choose"]; // these will later be used in the loop to 
  NumericVector Lambda = Hit3["Lambda"];// subset by DocID
  IntegerVector DocIDj = Hit3["DocIDj"];
  Function wich("which"); // call which() function from R
  Function conc("c"); //call c() function from R
  double oneUpdatedLambda = 0; //empty scalar
  NumericVector updatedLambdas = NumericVector::create(0); //empty vector to store lambdas
  for( int i=0; i<(DocIds.size()); ++i){
  int x = DocIds[i]; // loop through unique DocIds 
  LogicalVector matchedHits = docHit == x;// match wanted DocID to all DocIds
  NumericVector hitsIDs = as<NumericVector>(wich(Named("x")=matchedHits));//row index of matchedHits; check indices from R to rcpp
  DataFrame newData = DataFrame::create(Named("Choose") = Choose[hitsIDs-1], // -1 is for indexing
                                        Named("Lambda") = Lambda[hitsIDs-1], // which() indexes in R; loop indexes in c++
                                        Named("DocIDj") = DocIDj[hitsIDs-1]);
  oneUpdatedLambda = posteriorlambda(newData,extractLambda[i], 1, 1); //update lambda for each docID; newData is in the same order as extract lambda
  updatedLambdas = as<NumericVector>(conc(updatedLambdas, oneUpdatedLambda));
  }
  return updatedLambdas;
}


  