#' Bradley-Terry Updated Iterations
#'
#' Finds the probability of a student getting certain exam questions correctly given a parameter theta.
#'
#' @param a an a value
#' @param b a b value
#' @param id a vector of the document id numbers that need to be updated
#' @param lambda a data frame with document ids and corresponding lambda values. The columns must read 'DocId' and 'Lambda'
#' @param dataset a dataframe with three columns of data. The columns must read 'DocIDi', 'DocIDj', and 'Choose'.
#' @param iterations a number of the times you want lambda values updated
#'
#' @return a data frame of document ids and their corresponding updated lambdas
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
#' id<-seq(1:10)
#' 
#' bradleyterry(1,1,id,lambda,toydata2,20)
#' 
#' @rdname iterative.bt
#' @aliases bradleyterry, ANY-method
#' @export
iterative.bt<-function(a,b,id,lambda,dataset, iterations){
for (i in 1:iterations){
  lambda<-bradleyterry(a,b,id,lambda,dataset)}
return(lambda)
}