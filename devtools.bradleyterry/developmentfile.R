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

#### FUNCTION 1 #######
bradleyterry<-function(a,b,id,lambda,dataset){
  subsetdata<-dataset[dataset$DocIDi %in% id,]#this subsets the dataset down to just the observations with the id that we are looking at
  newlambda<-lambda[lambda$DocId %in% id,]#this extratcs the specific lambda amount we want to upgrade for the purpose of the equation 
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in subsetdata$DocIDj){
    lambdavec<-c(lambdavec,lambda$Lambda[i]) #This extracts all of the lambdaj values that we will work with in the equation below
  }
  for (i in 1:nrow(subsetdata)){
    sumunit<-(1/(newlambda$Lambda+lambdavec[i])) #This creates the summation term unit by unit with the lambda i value and all of the respective lambda js 
    sumvec<-as.vector(c(sumvec,sumunit)) #this makes a vector of the summation terms
  }
  summationterm<-sum(sumvec) #here we sum the terms of the vector to plug into the equation
  output<-(a-1+sum(subsetdata$Choose))/(b+summationterm) #this is where we finish up the equation and plug in all of our respective parts
  return(output)
}

#### FUNCTION 2 #######
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


#### FUNCTION 3 #######
iterative.bt<-function(a,b,id,lambda,dataset, iterations){
  for (i in 1:iterations){
    lambda<-bradleyterry.multid(a,b,id,lambda,dataset)}
  return(lambda)
}

bradleyterry(1,1,1,lambda,dataset)
bradleyterry.multid(1,1,id,lambda,dataset)
iterative.bt(1,1,id,lambda,dataset,1)

newlambda<-bradleyterry(1,1,id,lambda,dataset)
newlambda1<-bradleyterry(1,1,id,newlambda,dataset)
bradleyterry(1,1,1,newlambda1,dataset)
iterative.bt(1,1,id,lambda,dataset,1)
