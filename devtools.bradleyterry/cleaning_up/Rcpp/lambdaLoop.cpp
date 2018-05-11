#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame lambdaLoop(DataFrame hits, DataFrame lambdas, NumericVector DocIds, DataFrame Hit3, DataFrame extractLambdasDF){
  for( int i=0; i<DocIds.size(); ++i){
  int x=DocIds[i];
  Function wich("which");
  IntegerVector docHit= hits["DocIDi"];
  LogicalVector matchedHits= docHit== x;
  NumericVector hitsIDs= wich(Named("x")=matchedHits);
  IntegerVector Choose = Hit3["Choose"];
   NumericVector Lambda = Hit3["Lambda"];
   IntegerVector DocIDj = Hit3["DocIDj"];
   DataFrame newData = DataFrame::create(Named("Choose") = Choose[hitsIDs],
                                         Named("Lambda") = Lambda[hitsIDs],
                                         Named("DocIDj") = DocIDj[hitsIDs]);
  IntegerVector lambdaDocJ= lambdas["DocIDj"];
  //Function subset("subsetLambdasDF"); //should this be inside the function or as argument extractLambdasDF?
  //DataFrame extractLambda= subset(lambdas, DocIds);
  Function postLamb("posteriorlambda");
  DataFrame extractLambdasDF=postLamb(Named("newData")=newData,Named("docXprior")=extractLambdasDF, Named("a")=1, Named("b")=1);
  }
}


  