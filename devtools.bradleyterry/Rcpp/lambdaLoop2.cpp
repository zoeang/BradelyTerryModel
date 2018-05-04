#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame lambdaLoop2(DataFrame hits, DataFrame lambdas, NumericVector DocIds, DataFrame Hit3, DataFrame extractLambdasDF){
  
  IntegerVector docHit= hits["DocIDi"];
  IntegerVector Choose = Hit3["Choose"];
  NumericVector Lambda = Hit3["Lambda"];
  IntegerVector DocIDj = Hit3["DocIDj"];
  Function wich("which");
  Function postLamb("posteriorlambda");
  Function subset("subsetlambdas"); //should this be inside the function or as argument extractLambdasDF?
  double extractLambda= subset(lambdas, DocIds);
  Rcout << extractLambda;
  for( int i=0; i<DocIds.size(); ++i){
  int x=DocIds[i];
  LogicalVector matchedHits= docHit== x;
  Rcout << 1;
  NumericVector hitsIDs= wich(Named("x")=matchedHits);
  Rcout << 2;
  DataFrame newData = DataFrame::create(Named("Choose") = Choose[hitsIDs],
                                        Named("Lambda") = Lambda[hitsIDs],
                                        Named("DocIDj") = DocIDj[hitsIDs]);
  IntegerVector lambdaDocJ= lambdas["DocIDj"];
  Rcout << 3;
  DataFrame extractLambdasDF=postLamb(Named("newData")=newData,Named("docXprior")=extractLambda, Named("a")=1, Named("b")=1);
  Rcout << 4;
  extractLambda=extractLambdasDF;
  }
  return(1);
}


  