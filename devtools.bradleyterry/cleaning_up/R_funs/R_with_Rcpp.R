
allUpdatedLambda<-function(hits, lambdas, DocIds){
  HIT3<-merge(hits, lambdas, by="DocIDj") #this is an r function
  # below is a c++ function that calls the c++ function posteriorlambda
  for(i in 1:length(DocIds)){
    x<-DocIds[i]
    newData<-HIT3[which(hits$DocIDi==x), c("Choose", "Lambda", "DocIDj")] #nest an rcpp loop in an r loop?
    lambdas[lambdas$DocIDj==DocIds[i],2]<-posteriorlambda(newData,lambdas[lambdas$DocIDj==DocIds[i],2], a=1, b=1)
  }
  return(lambdas)
}