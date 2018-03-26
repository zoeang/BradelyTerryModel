#' Bradley-Terry
#'
#' Finds the probability of a student getting certain exam questions correctly given a parameter theta.
#'
#' @param a an a value
#' @param b a b value
#' @param lambdai a vector of lambda_i
#' @param lambdaj a vector of lambda_i
#' @param dataset a dataframe with three columns of data
#'
#' @return an output lambda_i^(t)
#'
#' @author Benjamin Schneider, Zoe Ang, and Hyun Woo Lim
#' @note This function produces vectors for use in finding the likelihood function.
#' @examples
#' 
#' lambdai<-runif(30)
#' lambdaj<-runif(30)
#' toydata<-data.frame(rep(1,30), sample(1:100, 30, replace=T), sample(c(0,1), 30, replace=T))
#' colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
#' bradleyterry(1,2,lambdai,lambdaj,toydata)
#' 
#' @rdname bradleyterry
#' @aliases bradleyterry, ANY-method
#' @export
bradleyterry<-function(a,b,lambdai,lambdaj,dataset){
  summationterm<-0
  for (i in nrow(dataset$DocIDj)){
    summationterm<-summationterm+(1/(lambdai[i]+lambdaj[i]))
  }
  output<-(a-1+sum(dataset$Choose))/(b+summationterm)
  return(output)
}
