tolTest<- function(hits, lambdas, DocIds, iterations){
  lambda1<-lambdas$Lambda
  hits$Lambda <- lambdas$Lambda[match(hits$DocIDj, lambdas$DocId)]
  for( i in 1:iterations){ #change arguments of lambdas
    #HIT3<-merge(hits, lambdas, by="DocIDj") 
    lambda1<-lambdaLoop2(hits=HIT2, DocIds = DocId, extractLambda=lambda1)
    lambdas$Lambda<-lambda1<-lambda1[-1]
    hits$Lambda <- lambdas$Lambda[match(hits$DocIDj, lambdas$DocId)]
    if (all(abs(lambda1-lambdas$Lambda)<1e-15)){
      break
    }
    else{
      lambdas$Lambda<-lambda1
    }
<<<<<<< HEAD
    print(lambda1)
=======
    print(i)
>>>>>>> 3c135d3daff308d7dafb090fe6e67edd65999f5e
  }
  return(lambda1)
  
}