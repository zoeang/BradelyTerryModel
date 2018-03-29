rm(list=ls())
library(devtools)
library(roxygen2)

setwd("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry")
current.code<-as.package("bradleyterry")
load_all(current.code)
document(current.code)
test(current.code)

docjdat<-c(2,seq(2,10,1), 1, seq(3,10,1), 1,2, seq(4,10,1),1, seq(1,3,1), seq(5,10,1),
          1, seq(1,4,1), seq(6,10,1),
           1,3,seq(1,5,1), seq(7,10,1),
           1, 3,seq(1,6,1), seq(8,10,1),
           1, 3,seq(1,7,1), seq(9,10,1),
          1, 3, seq(1,9,1),1, seq(2,8,1))
rep(c(1,0,1,1,0),20)
#CREATE FAKE DATA
dataset<-toydata<-data.frame(sort(rep(seq(1,10),10)), docjdat, rep(c(1,0,1,1,0),20))
colnames(dataset)<-c("DocIDi", "DocIDj", "Choose")
dataset
table(toydata$DocIDj)

#Remove rows where doci=docj
#this is not irrelevant
#toydata$delete<- (toydata$DocIDi==toydata$DocIDj)
#toydata2<-subset(toydata,delete=="FALSE")
#toydata2<-toydata2[,-4]
#dataset<-toydata2


#create lambda data frame
lambda<-data.frame(c(10:1),runif(10))
colnames(lambda)<-c('DocId', 'Lambda')

library(plyr)
ddply(toydata2,2)
#ply example from Lecture 8====================================================
dd<-data.frame(matrix(rnorm(216),72,3),c(rep("A",24),rep("B",24),rep("C",24)),c(rep("J",36),rep("K",36)))
colnames(dd) <- c("v1", "v2", "v3", "dim1", "dim2")
head(dd)
dd
obj1 <- ddply(.data=dd, .variables=c("dim1","dim2"), .fun=function(df) mean(df$v1))
obj1
#maybe?
ddply(.data=dataset, .variables=c("DocIDi", "DocIDj"), .fun=bradleyterry(a,b,id,lambda,dataset))
#end example===================================================================

bradleyterry<-function(a,b,id,lambda,dataset){
  subsetdata<-dataset[dataset$DocIDi %in% id,]
  newlambda<-lambda[lambda$DocId %in% id,]
  sumvec<-NULL
  lambdavec<-NULL
  for(i in subsetdata$DocIDj){
    lambdavec<-c(lambdavec,lambda$Lambda[i])
  }
  for (i in 1:nrow(subsetdata)){
    sumunit<-(1/(newlambda$Lambda+lambdavec[i]))
    sumvec<-as.vector(c(sumvec,sumunit))
  }
  summationterm<-sum(sumvec)
  output<-(a-1+sum(subsetdata$Choose))/(b+summationterm)
  return(output)
}

id<-rep(1:10)
dataset
lambda<-data.frame(c(10:1),.5)
colnames(lambda)<-c('DocId', 'Lambda')

bradleyterry(1,1,id,lambda,dataset)

bradleyterry<-function(a,b,id,lambda,dataset){
  updatedlambda<-NULL
  for (i in id){
    subsetdata<-dataset[dataset$DocIDi %in% i,]
    newlambda<-lambda[lambda$DocId %in% i,]
    sumvec<-NULL
    lambdavec<-NULL
    for(i in subsetdata$DocIDj){
      lambdavec<-c(lambdavec,lambda$Lambda[i])
    }
    for (i in 1:nrow(subsetdata)){
      sumunit<-(1/(newlambda$Lambda+lambdavec[i]))
      sumvec<-as.vector(c(sumvec,sumunit))
    }
    summationterm<-sum(sumvec)
    output<-(a-1+sum(subsetdata$Choose))/(b+summationterm)
    updatedlambda<-c(updatedlambda,output)}
  output<-cbind(id,updatedlambda)
  output<-as.data.frame(output)
  colnames(output)<-c('DocId','Lambda')
  return(output)
}
