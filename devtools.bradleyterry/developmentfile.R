rm(list=ls())
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
colnames(lambda)<-c('DocId', 'Lambda')

bradleyterry(1,1,lambdai,lambdaj,toydata)

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
  sumvec<-NULL
  for (i in 1:nrow(subsetdata)){
    sumunit<-(1/(lambda$Lambda[id]+lambdaj[i]))
    sumvec<-as.vector(c(sumvec,sumunit))
  }
  summationterm<-sum(sumvec)
  output<-(a-1+sum(dataset$Choose))/(b+summationterm)
  return(output)
}