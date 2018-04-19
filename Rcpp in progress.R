library(Rcpp)

setwd('C:/Users/dell/Documents/GitHub/BradelyTerryModel')
sourceCpp('rcpp_ex2.cpp')

data.gen<-function(NumericVector lambda,NumericVector n)
  ouput.lambda<- R_Nilvalue
  for(int i=0; i<n; i++){
    lams<-csample_num(lambda[1], int 2)
    lambdavec<-R_Nilvalue
    for (int i=0; i<n;i++){
      labmbdavec<-c(labmbdavec,lambda[2][i])
    }
    prob<-lambdavec[1]/(lambdavec[1]+lambdavec[2])
    choose<-csample_num(c(0,1), int 1, bool replace,NumericVector prob = NumericVector::create())
  
    })
)


cppFunction(
  Brad<-function(NumericVector lambda,NumericVector b,NumericVector id,NumericVector lambda,dataset )
  subsetdata<-ex_fun1(dataset,id)
  newlambda<-ex_fun2(lambda,id)
  sumvec<-R_Nilvalue
  lambdavec<-R_Nilvalue
  n<-nrows(subsetdata[DocIDi])
  for(int i=0; i<n; i++){
    lambdajsubset<-ex_fun2[lambda[DocIDi], subsetdata[DocIDi][i]]

/*** R
timesTwo(42)


NumericVector v1 = k[1];
DataF
class(k)
library(Rcpp)
setwd('C:/Users/dell/Documents/GitHub/BradelyTerryModel')
sourceCpp('rcpp_ex2.cpp')
*/