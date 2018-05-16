tolTest<- function(hits, lambdas, DocIds, iterations){
  lambda1<-lambdas$Lambda
  for( i in 1:iterations){ #change arguments of lambdas
    HIT2$Lambda <- lambda$Lambda[match(HIT2$DocIDj, lambda$DocId)]
    #HIT3<-merge(hits, lambdas, by="DocIDj") 
    lambda1<-lambdaLoop2(hits=HIT2, DocIds = DocId,Hit3 = HIT2, extractLambda=lambda1)
    lambda1<-lambda1[-1]
    if (all(abs(lambda1-lambdas$Lambda)<1e-15)){
      break
    }
    else{
      lambdas$Lambda<-lambda1
    }
    print(lambda1)
  }
  return(lambda1)
  
}