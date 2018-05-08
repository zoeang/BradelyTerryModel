#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame lambdaLoop2(DataFrame hits, DataFrame lambdas, NumericVector DocIds, DataFrame Hit3){
  
  IntegerVector docHit= hits["DocIDi"];
  IntegerVector Choose = Hit3["Choose"];
  NumericVector Lambda = Hit3["Lambda"];
  IntegerVector DocIDj = Hit3["DocIDj"];
  Function wich("which");
  Function postLamb("posteriorlambda");
  Function subset("getlambda"); //should this be inside the function or as argument?
  NumericVector extractLambda = subset(lambdas, DocIds);
  for( int i=0; i<DocIds.size(); ++i){
  int x = DocIds[i];
  LogicalVector matchedHits = docHit == x;
  NumericVector hitsIDs = wich(Named("x")=matchedHits);
  DataFrame newData = DataFrame::create(Named("Choose") = Choose[hitsIDs],
                                        Named("Lambda") = Lambda[hitsIDs],
                                        Named("DocIDj") = DocIDj[hitsIDs]);
  //double extractLambdasDF = postLamb(newData,extractLambda[i], 1, 1);//output is the updated lambda; where to save? back into extractLambda
  Rcout <<4;
  //extractLambda[i]=extractLambdasDF;
  }
  return("Le fin");
}


  