rm(list=ls())
library(devtools)
library(roxygen2)

setwd("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry")
setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry")
current.code<-as.package("bradleyterry")
load_all(current.code)
document(current.code)
test(current.code)

#Data Generating Function======================================================
#the function creates data using lambda values to generate actualized values 
#of a document "winning" (Choose)
set.seed(42)
id<-1:10
Lam<-runif(10)
lambda<-as.data.frame(cbind(id,Lam))
colnames(lambda)<-c('DocId', 'Lambda')
lam<-runif(10)
lambda1<-as.data.frame(cbind(id,.5))
colnames(lambda1)<-c('DocId', 'Lambda')

data.generation<-function(lambda,n){ #n size dataset
  output.lambda<-NULL #creates a template for the output dataset
  for (i in 1:n){#this is a for loop for creating data points, n size dataset
    lams<-sample(lambda$DocId, 2)#this randomly selects two of the lambdas at random
    lambdavec<-NULL #creates a templace for extracting the lambda values
    for (i in lams){
      lambdavec<-c(lambdavec,lambda$Lambda[i]) #this actually extracts the lambdas
    }
    prob<-lambdavec[1]/(lambdavec[1]+lambdavec[2]) #this uses the lambdas in order to create a probability for selection
    Choose <- sample(c(0,1), 1, replace = TRUE, prob = c(1-prob, prob))#this chooses the document with the predetermined probability
    new.lambda<-cbind(lams[1],lams[2],Choose) #now building our output
    output.lambda<-rbind(output.lambda, new.lambda)#row binding with the template
  }
  rownames(output.lambda)<-NULL #getting rid of the numbers for row name
  output.lambda<-as.data.frame(output.lambda) #we do this because removing our row names made a sort of matrix
  colnames(output.lambda)<-c("DocIDi","DocIDj","Choose") #Making the output like our dataset
  return(output.lambda) #outputs our data
}

dataset<-data.generation(lambda,750)

newlam1<-iterative.bt(1,1,id,lambda1,dataset,2000)
sum(lambda$Lambda-newlam$Lambda)

#==============================================================================
#==============================================================================

#### FUNCTION 1 #######
bradleyterry<-function(a,b,id,lambda,dataset){
  subsetdata<-dataset[dataset$DocIDi %in% id,]#this subsets the dataset down to just the observations with the id that we are looking at
  newlambda<-lambda[lambda$DocId %in% id,]#this extracts the specific DocID and lambda value we want to upgrade for the purpose of the equation 
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in 1:nrow(subsetdata)){ #the purpose of thsi loop is to extract lambda j values for use in the next loop
    lambdajsubset<-lambda[lambda$DocId %in% subsetdata$DocIDj[i],] #This picks out the lambda j values for each of the elements of the subset dataset
    lambdavec<-c(lambdavec,lambdajsubset$Lambda) #this building the vector for use
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
  updatedlambda<-NULL #creating a vector for storing the updated lambda
  for (i in id){ # run loop for each vector in id
    newlambda<-bradleyterry(a,b,i,lambda,dataset) #running the function above for each chosen doc id
    updatedlambda<-c(updatedlambda,newlambda) #update lambda
  }
  lambdajsave<-lambda[!lambda$DocId %in% id,]
  output<-as.data.frame(cbind(id,updatedlambda)) #bind id and updated lambda as dataframe
  #output<-as.data.frame(output) #original code; moved to row above #putting the output in a format for later use
  colnames(output)<-c('DocId','Lambda') #naming the outputs so they can be included right back in
  return(output)
}
bradleyterry.multid(1,0,id=c(3,4), lambda, dataset)
#### FUNCTION 3 #######
iterative.bt<-function(a,b,id,lambda,dataset, iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda<-bradleyterry.multid(a,b,id,lambda,dataset) #run the code above for one doc id, a number of times determined by user
    }
  return(lambda) #returns the output as the number of iterations determined by the user.
}


#================================================================================
#================================================================================

#### DATA JACOB GAVE US
HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
HIT<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
head(HIT)

id<-unique(HIT$DocIDi) 

lambda<-data.frame(c(10:1),runif(10))
colnames(lambda)<-c('DocId', 'Lambda')
HIT$lambda<-runif(nrow(HIT))
up_lambda<-bradleyterry(1,2,1922,lambda,HIT)
newlambda<-lambda[lambda$DocId %in% id,]

#Dataset to recover lambda############ #IF THIS PART IS DATA GENERATING PART, I THINK THIS DOES WHAT WE WANT TO DO. (LIM)
HIT #start with HIT data
HIT$lambda_i<-rep(runif(50), each=2) #create 100 random values for lambda_i # THIS ONLY WORKS IF LAMBDA IS IS SORTED.
#I THINK WE NEED TO FIND A BETTER WAY OTHERWISE, WE HAVE TO SORT DATA BEFORE RUN A FUNCTION. (LIM)

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
  
  #trials with Jacob's data====================================
  bradleyterry(1,1,1,lambda,dataset)
  bradleyterry.multid(1,1,HIT$DocIDi,lambda,HIT)
  iterative.bt(1,1,HIT$DocIDi,lambda,HIT,1)
  
  newlambda<-bradleyterry(1,1,HIT$DocIDi,lambda,HIT)
  newlambda1<-bradleyterry(1,1,DocIDi,newlambda,HIT)
  bradleyterry(1,1,1,newlambda1,HIT)
  iterative.bt(1,1,HIT$DocIDi,lambda,HIT,1)
  
  load('/Users/benjaminschneider/Downloads/docInfo.Rdata') # I dont get this part. (LIM)
  head(docInfo)
}
