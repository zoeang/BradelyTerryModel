#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame lambdaLoop(DataFrame hits, DataFrame lambdas, NumericVector DocIds, DataFrame Hit3){
  for( int i=0; i<DocIds.size(); ++i){
  int x=DocIds[i];
  Function wich("which");
  LogicalVector matchedHits=as<int>hits["DocIDi"]== as<int>x;
  NumericVector hitsIDs= wich(Named("x")=matchedHits);  //'which ' equivalent in rcpp
  Function con("c");
  DataFrame newData= Hit3[hitsIDs, con("Choose", "Lambda", "DocIDj")];
  IntegerVector lambdaDocJ= lambdas["DocIDj"];
  Function subset("subsetLambdasDF"); //should this be inside the function?
  DataFrame extractLambda= subset(lambdas, DocIds);
  Function postLamb("posteriorlambda");
  DataFrame extractlambdas=postLamb(Named("newData")=newData,Named("docXprior")=extractlambdas, Named("a")=1, Named("b")=1);
  // how to subset DF?
  }
}


  