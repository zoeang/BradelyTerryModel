#include<Rcpp.h>


using namespace Rcpp;


// [[Rcpp::export]]
double posteriorlambda(DataFrame newData, double docXprior, int a=1, int b=1){
  double numerator = (a-1) + sum(as<NumericVector>(newData["Choose"]));
  double denominator = (b + sum(1 / (docXprior + as<NumericVector>(newData["Lambda"]))));
  //  Rcout << numerator;
  //  Rcout << denominator;
  //  Rcout << numerator/denominator;
  return numerator/denominator;
}

// [[Rcpp::export]]
NumericVector lambdaLoop2(DataFrame hits, NumericVector DocIds, NumericVector extractLambda){
  IntegerVector docHit= hits["DocIDi"]; //create vectors from the columns of the dataframe
  IntegerVector Choose = hits["Choose"]; // these will later be used in the loop to 
  NumericVector Lambda = hits["Lambda"];// subset by DocID
  IntegerVector DocIDj = hits["DocIDj"];
  Function wich("which");
  Function conc("c");
  //Function postLamb("posteriorlambda"); //if calling from a different .cpp
 // Function subset("getlambda"); 
 // NumericVector extractLambda = subset(lambdas, DocIds);// [[Rcpp::export]]
  //Rcout<< extractLambda;
  double updatedLambda = 0;
  NumericVector updatedLambdas = NumericVector::create(0);
  for( int i=0; i<(DocIds.size()); ++i){
  int x = DocIds[i];
  LogicalVector matchedHits = docHit == x;// match wanted DocID to all DocIds
  NumericVector hitsIDs = as<NumericVector>(wich(Named("x")=matchedHits));//row index of matchedHits; check indices from R to rcpp
  DataFrame newData = DataFrame::create(Named("Choose") = Choose[hitsIDs-1],
                                        Named("Lambda") = Lambda[hitsIDs-1],
                                        Named("DocIDj") = DocIDj[hitsIDs-1]);
  Rcout<<newData;
  updatedLambda = posteriorlambda(newData,extractLambda[i], 1, 1);
  //Rcout << updatedLambda; 
  updatedLambdas = as<NumericVector>(conc(updatedLambdas, updatedLambda));
  }
  return updatedLambdas;
}


  