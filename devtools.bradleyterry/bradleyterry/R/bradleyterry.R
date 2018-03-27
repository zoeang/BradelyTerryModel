#' Bradley-Terry
#'
#' Finds the probability of a student getting certain exam questions correctly given a parameter theta.
#'
#' @param a an a value
#' @param b a b value
#' @param id the row of your lambda dataframe with the document id that you want an updated lambda value for
#' @param lambda a data frame with document ids and corresponding lambda values. The columns must read 'DocId' and 'Lambda'
#' @param dataset a dataframe with three columns of data. The columns must read 'DocIDi', 'DocIDj', and 'Choose'.
#'
#' @return a single output value for lambda_i^(t)
#'
#' @author Benjamin Schneider, Zoe Ang, and Hyun Woo Lim
#' @note This function produces vectors for use in finding the likelihood function.
#' @examples
#' 
#' toydata<-data.frame(sort(rep(seq(1,10),10)), sample(1:10, 100, replace=T), sample(c(0,1), 100, replace=T))
#' colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
#' toydata$delete<- (toydata$DocIDi==toydata$DocIDj)
#' toydata2<-subset(toydata,delete=="FALSE")
#' toydata2<-toydata2[,-4]
#' lambda<-data.frame(c(1:10),runif(10))
#' colnames(lambda)<-c('DocId', 'Lambda')
#' 
#' bradleyterry(1,1,1,lambda,toydata2)
#' 
#' @rdname bradleyterry
#' @aliases bradleyterry, ANY-method
#' @export
bradleyterry<-function(a,b,id,lambda,dataset){
  subsetdata<-dataset[dataset$DocIDi %in% id,]
  sumvec<-NULL
  lambdavec<-NULL
  for(i in subsetdata$DocIDj){
    lambdavec<-c(lambdavec,lambda$Lambda[i])
  }
  for (i in 1:nrow(subsetdata)){
    sumunit<-(1/(lambda$Lambda[id]+lambdavec[i]))
    sumvec<-as.vector(c(sumvec,sumunit))
  }
  summationterm<-sum(sumvec)
  output<-(a-1+sum(subsetdata$Choose))/(b+summationterm)
  return(output)
}
