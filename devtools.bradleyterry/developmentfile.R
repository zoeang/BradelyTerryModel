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
#consider for row 44
which(HIT$DocIDj==subsetdata$DocIDj) #this gives the rows of HIT of the relevant docs


#### FUNCTION 1 #######
bradleyterry<-function(a,b,id,lambda,dataset){
  subsetdata<-dataset[dataset$DocIDi %in% id,]#this subsets the dataset down to just the observations with the id that we are looking at
  newlambda<-lambda[lambda$DocId %in% id,]#this extratcs the specific lambda amount we want to upgrade for the purpose of the equation 
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in 1:length(subsetdata$DocIDj)){
    lambdajsubset<-lambda[lambda$DocId %in% subsetdata$DocIDj[i],]
    lambdavec<-c(lambdavec,lambdajsubset$Lambda)
    }
  for (i in 1:nrow(subsetdata)){
    sumunit<-(1/(newlambda$Lambda+lambdavec[i])) #This creates the summation term unit by unit with the lambda i value and all of the respective lambda js 
    sumvec<-as.vector(c(sumvec,sumunit)) #this makes a vector of the summation terms
  }
  summationterm<-sum(sumvec) #here we sum the terms of the vector to plug into the equation
  output<-(a-1+sum(subsetdata$Choose))/(b+summationterm) #this is where we finish up the equation and plug in all of our respective parts
  return(output)
}

lambdajsubset<-lambda[lambda$DocId %in% subsetdata$DocIDj[2],]
lambdavec<-c(lambdavec,lambdajsubset$Lambda)
nrow(q)
dataset<-metaHIT
subsetdata<-dataset[dataset$DocIDi %in% 5059,]
newlambda<-lambda[lambda$DocId %in% 5059,]

bradleyterry(1,0,5059,lambda,metaHIT)
bradleyterry.multid(1,0,id,lambda,metaHIT)

lambda[lambda$DocId %in% subsetdata$DocIDj,]

nums<-rownames(lambdajsubset)
as.numeric(nums)

?which
which(lambda$DocId==subsetdata$DocIDj)
HIT
lambda
subsetdata<-HIT[HIT$DocIDi %in% 1786,]
newlambda<-lambda[lambda$DocId %in% 1786,]
bradleyterry(1,0,1,lambda,HIT)
id<-HIT$DocIDi[1:100]
id<-lambda$DocId
bradleyterry(1,0,5059,lambda,metaHIT)
bradleyterry.multid(1,0,id,lambda,metaHIT)

HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
HIT<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
HIT

id<-unique(HIT$DocIDi) 

lambda<-data.frame(c(10:1),runif(10))
colnames(lambda)<-c('DocId', 'Lambda')
HIT$lambda<-runif(nrow(HIT))
up_lambda<-bradleyterry(1,2,1922,lambda,HIT)
newlambda<-lambda[lambda$DocId %in% id,]

#Dataset to recover lambda############
HIT #start with HIT data
HIT$lambda_i<-rep(runif(50), each=2) #create 100 random values for lambda_i

#fake to match docid_j
DocIDj<-unique(HIT$DocIDj)
lambda_j<-runif(length(unique(HIT$DocIDj)))
fake<-cbind(DocIDj,lambda_j)

#merge
HIT <- merge(fake,HIT,by="DocIDj")


#probablity of choosing doc_i over doc_j
HIT$p_i<-HIT$lambda_i/(HIT$lambda_i+HIT$lambda_j) #calculate the prob that doc i beats doc j
HIT$p_i_not<-1-HIT$p_i #compliment of P(i beats J)


#choosing what is chosen
for (i in 1:nrow(HIT)){
  #HIT$chosen[i]<-rbinorm(c(1,0),1,replace=T,prob=c(HIT$p_i[i],HIT$p_i_not[i]))
  HIT$chosen[i]<-rbinom(1,1,prob=c(HIT$p_i[i],HIT$p_i_not[i])) #rbinom simulation
}


#### FUNCTION 2 #######
bradleyterry.multid<-function(a,b,id,lambda,dataset){
  updatedlambda<-NULL
  for (i in id){ # run loop for each vector in id
    newlambda<-bradleyterry(a,b,i,lambda,dataset)
    updatedlambda<-c(updatedlambda,newlambda) #update lambda
  }
  lambdajsave<-lambda[!lambda$DocId %in% id,]
  output<-cbind(id,updatedlambda) #bind id and updated lambda
  output<-as.data.frame(output)
  #output<-rbind(output,lambdajsave)
  colnames(output)<-c('DocId','Lambda')
  return(output)
}



lambdajsave<-lambda[!lambda$DocId %in% id,]


#### FUNCTION 3 #######
iterative.bt<-function(a,b,id,lambda,dataset, iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda<-bradleyterry.multid(a,b,id,lambda,dataset)}
  return(lambda)
}

bradleyterry(1,1,1,lambda,dataset)
bradleyterry.multid(1,1,id,lambda,dataset)
x<-iterative.bt(1,0,id,lambda,metaHIT1,100)
head(x)
head(lambda)

id
Lam<-runif(300,0,10)
lambda<-as.data.frame(cbind(id,Lam))
colnames(lambda)<-c('DocId', 'Lambda')

newlambda<-bradleyterry(1,1,id,lambda,dataset)
newlambda1<-bradleyterry(1,1,id,newlambda,dataset)
bradleyterry(1,1,1,newlambda1,dataset)
iterative.bt(1,1,id,lambda,dataset,1)

#trials with Jacob's data====================================
bradleyterry(1,1,1,lambda,dataset)
bradleyterry.multid(1,1,HIT$DocIDi,lambda,HIT)
iterative.bt(1,1,HIT$DocIDi,lambda,HIT,1)

newlambda<-bradleyterry(1,1,HIT$DocIDi,lambda,HIT)
newlambda1<-bradleyterry(1,1,DocIDi,newlambda,HIT)
bradleyterry(1,1,1,newlambda1,HIT)
iterative.bt(1,1,HIT$DocIDi,lambda,HIT,1)

load('/Users/benjaminschneider/Downloads/docInfo.Rdata')
head(docInfo)




#####DATA GENERATING FUNCTION

data.generation<-function(lambda,n){
  output.lambda<-NULL
  for (i in 1:n){
    
    
    output.lambda<-rbind(output.lambda, new.lambda)
  }
  colnames(output.lambda)<-c("DocIDi","DocIDj","Choose")
  return(output.lambda)
}
