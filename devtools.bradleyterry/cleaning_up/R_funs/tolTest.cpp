tolTest<- function(hits, lambdas, DocIds, iterations){
  lambda1<-lambdas$Lambda
  for( i in 1:iterations){ #change arguments of lambdas
    HIT3<-merge(hits, lambdas, by="DocIDj") 
    lambda1<-lambdaLoop2(hits=HIT2, DocIds = DocId,Hit3 = HIT3, extractLambda=lambda1)
    
    lambda1<-lambda1[-1]
    //print(c(lambda1, "here"))
    //lambdas$Lambda<-lambda1[-1]
    
    if (all(abs(lambda1-lambdas$Lambda)<1e-15)){
      break
    }
    else{
      lambdas$Lambda<-lambda1
    }
  }
  return(lambda1)
  
}