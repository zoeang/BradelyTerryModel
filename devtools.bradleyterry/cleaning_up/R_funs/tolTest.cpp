tolTest<- function(hits, lambdas, DocIds, iterations){
  lambda1<-lambdas$Lambda
  for( i in 1:iterations){ #change arguments of lambdas
    HIT3<-merge(hits, lambdas, by="DocIDj") 
    lambda1<-lambdaLoop2(hits=HIT2, DocIds = DocId,Hit3 = HIT2, extractLambda=lambda1)
    
    lambda1<-lambda1[-1]
    //print(c(lambda1, "here"))
    //lambdas$Lambda<-lambda1[-1]
    
<<<<<<< HEAD:devtools.bradleyterry/cleaning_up/R_funs/toltest_Rcpp.R
    if (all(abs(lambda1-lambdas$Lambda)<1e-8)){
=======
    if (all(abs(lambda1-lambdas$Lambda)<1e-15)){
>>>>>>> 5a7176c108b5bc2456ced915861172ae48e7a297:devtools.bradleyterry/cleaning_up/R_funs/tolTest.cpp
      break
    }
    else{
      lambdas$Lambda<-lambda1
    }
  }
  return(lambda1)
  
}