DataFrame bradleyterryeasy(int a, int b,NumericVector id, DataFrame lambda, DataFrame dataset){//rename bradleyterry.easy
  NumericVector sumvec=;
  NumericVector lambdavec=;
  for ( int i; subsetdata.nrow(); ++i{
    double sumunit=(1/(newlambda['Lambda']+lambdavec[i])) ;
    NumericVector sumvec=c(sumvec,sumunit));
  }
 int summationterm=sum(sumvec) ;
   DataFrame output=(a-1+sum(subsetdata['Choose']))/(b+summationterm);
    return(output)
}

// sugar
SEXP btmult(SEXP a, b, id, lambda, dataset){
  DataFrame output=sapply(id, function(x) bradleyterryeasy(a,b,id=x, lambda, dataset))
  DataFrame output<-cbind(id,output)
  colnames(output)<-c('DocId','Lambda')
  return(output)
}
