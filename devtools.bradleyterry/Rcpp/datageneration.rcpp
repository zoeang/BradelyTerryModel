library(Rcpp)
/*** subset a matrix with (), dataframe []?
cppFunction('DataFrame data.generation(DataFrame lambda; int n){
 DataFrame output.lambda = ;
 for(int i=0; i<n; i++){
 NumericVector lams= sample(lambda['DocID'], 2);
 NumericVector lambdavec = ;
  for(int i=0; lams.size(); ++i){
    lambdavec= c(lambdavec, lambda['Lambda'][i]);
    };
 double prob= lambdavec[0]/(lambdavec[0]+lambdavec[1]);
 NumericVector Choose = sample (c(0,1),1, replace=T, prob= c(1-prob, prob));
 NumerixMatrix new.lambda = cbind(lams[0], lms[1], Choose);
 NumericMatrix output.lambda = rbind(output.lambda, new.lambda);
 };
 DataFrame output.lambda = as.data.frame(output.lambda);
 colnames(output.lambda) = c('DocIDi', 'DocIDj', 'Choose');
 return(output.lambda)
 }')