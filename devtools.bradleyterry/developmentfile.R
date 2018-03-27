library(devtools)
library(roxygen2)

setwd("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry")
current.code<-as.package("bradleyterry")
load_all(current.code)
document(current.code)
test(current.code)


#CREATE FAKE DATA
toydata<-data.frame(sort(rep(seq(1,10),10)), sample(1:10, 100, replace=T), sample(c(0,1), 100, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
toydata
table(toydata$DocIDj)

#Remove rows where doci=docj
toydata$delete<- (toydata$DocIDi==toydata$DocIDj)
toydata2<-subset(toydata,delete=="FALSE")
toydata2<-toydata2[,-4]
toydata2

#create lambda data frame
lambda<-data.frame(c(1:10),runif(10))
lambda

bradleyterry(1,1,lambdai,lambdaj,toydata)

bradleyterry<-function(a,b,i,lambda,dataset){
  sumvec<-NULL
  
  for (i in 1:length(lambdai)){
    sumunit<-(1/(lambdai[i]+lambdaj[i]))
    sumvec<-as.vector(c(sumvec,sumunit))
  }
  summationterm<-sum(sumvec)
  output<-(a-1+sum(dataset$Choose))/(b+summationterm)
  return(output)
}