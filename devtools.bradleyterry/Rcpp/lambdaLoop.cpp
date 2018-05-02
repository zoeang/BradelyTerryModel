allUpdatedLambda<-function(hits, lambdas, DocIds){
  HIT3<-merge(hits, lambdas, by="DocIDj")
  
  for(i in 1:length(DocIds)){
    x<-DocIds[i]
    newData<-HIT3[which(hits$DocIDi==x), c("Choose", "Lambda", "DocIDj")]
    lambdas[lambdas$DocIDj==DocId[i],2]<-posteriorlambda(newData,lambdas[lambdas$DocIDj==DocId[i],2], a=1, b=1)
  }
  return(lambdas)
}

#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame lambdaLoop(DataFrame hits, DataFrame lambdas, NumericVector DocIds, DataFrame Hit3){
  for( int i=0; i<DocIds.size(); ++i){
  int x=DocIds[i];
  DataFrame newData= Hit3[which(hits['DocIDi']==x), c("Choose", "Lambda", "DocIDj")];
  LogicalVector lambdaDocJ= lambdas['DocIDj'];
  Function postLamb("porteriorlambda");
  DataFrame lambdas[lambdaDocJ==DocIds[i],1]=postLamb(newData,lambdas[lambdaDocJ==DocID[i],1], a=1, b=1 );
  }
}