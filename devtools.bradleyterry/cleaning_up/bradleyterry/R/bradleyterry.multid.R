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
  updatedlambda<-NULL #creating a vector for storing the updated lambda
  for (i in id){ # run loop for each vector in id
    newlambda<-bradleyterry(a,b,i,lambda,dataset) #running the function above for each chosen doc id
    updatedlambda<-c(updatedlambda,newlambda) #update lambda
  }
  lambdajsave<-lambda[!lambda$DocId %in% id,]
  output<-cbind(id,updatedlambda) #bind id and updated lambda
  output<-as.data.frame(output) #putting the output in a format for later use
  colnames(output)<-c('DocId','Lambda') #naming the outputs so they can be included right back in
  return(output)
}