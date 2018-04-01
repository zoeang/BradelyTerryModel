#' Bradley-Terry of multiple ids
#'
#' Finds the probability of a student getting certain exam questions correctly given a parameter theta.
#'
#' @param a an a value
#' @param b a b value
#' @param id a vector of ids
#' @param lambda a data frame with document ids and corresponding lambda values. The columns must read 'DocId' and 'Lambda'
#' @param dataset a dataframe with three columns of data. The columns must read 'DocIDi', 'DocIDj', and 'Choose'
#'
#' @return a dataframe of updated lambda values 
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
#' bradleyterry.multid(1,1,id,lambda,toydata2)
#' 
#' @rdname bradleyterry.multid
#' @aliases bradleyterry, ANY-method
#' @export
bradleyterry.multid<-function(a,b,id,lambda,dataset){
  updatedlambda<-NULL
  for (i in id){
    newlambda<-bradleyterry(a,b,i,lambda,dataset)
    updatedlambda<-c(updatedlambda,newlambda)
  }
  output<-cbind(id,updatedlambda)
  output<-as.data.frame(output)
  colnames(output)<-c('DocId','Lambda')
  return(output)
}