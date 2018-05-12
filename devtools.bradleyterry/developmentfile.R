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

#Data Generation===============================================================
dat<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)


HIT<-dat[,3:5]

HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
HIT<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)

datatransform<-function(HIT){
  vec<-rep(1:2, 1500)
  HIT<-as.data.frame(cbind(HIT,vec))
  colnames(HIT)<-c("comparison_id" ,"document_id", "result","num")
  DocIDi<-NULL
  DocIDj<-NULL
  Choose<-NULL
  for (i in 1:nrow(HIT)){
    if (HIT$num[i]==2){
      DocIDj<-c(DocIDj,HIT$document_id[i])
    }
    else{
      DocIDi<-c(DocIDi,HIT$document_id[i])
      Choose<-c(Choose,HIT$result[i])
    }
  }
  newHIT<-as.data.frame(cbind(DocIDi,DocIDj,Choose))
  DocIDi<-NULL
  DocIDj<-NULL
  Choose<-NULL
  for (i in 1:nrow(HIT)){
    if (HIT$num[i]==1){
      DocIDj<-c(DocIDj,HIT$document_id[i])
    }
    else{
      DocIDi<-c(DocIDi,HIT$document_id[i])
      Choose<-c(Choose,HIT$result[i])
    }
  }
  newHIT2<-as.data.frame(cbind(DocIDi,DocIDj,Choose))
  metaHIT<-rbind(newHIT,newHIT2)
  return(metaHIT)
}

HIT2<-datatransform(HIT)

set.seed(42)
DocId<-unique(HIT$document_id)
DocId<-sort(DocId, decreasing = F)
Lambda<-runif(50)
lambda<-cbind(DocId,Lambda)
lambda<-as.data.frame(lambda)


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

dataset<-data.generation(lambda,500)

#==============================================================================
#Pre-format data prior to rcpp coding
#==============================================================================
head(HIT2)
datatrans<-function(docid,dat){
  outputlist<-NULL
  outputlist<-as.list(outputlist)
  for (i in docid){  
    outputlist[[i]]<-dat[dat$DocIDi %in% i,]
  }
  return(outputlist)
}
test1<-datatrans(DocId,HIT2)
test1[[5000]]

lambdatrans<-function(docid,lambda){
  lambdalist<-NULL
  lambdalist<-as.list(lambdalist)
  for (i in docid){  
    lambdalist[[i]]<-lambda[lambda$DocId %in% i,]
  }
  return(lambdalist)
}
head(lambda)
test2<-lambdatrans(DocId,lambda)
test2[[4990]][,2]
 class(test2)
#### FUNCTION 1 #######

#This function uses the pre-formatted 'lambda' and 'dataset': use for rcpp 
bradleyterryeasy<-function(a,b,id,lambda,dat){
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in 1:nrow(dat[[id]])){ #the purpose of thsi loop is to extract lambda j values for use in the next loop
    lambdajsubset<-dat[[id]]$DocIDj[i] #This picks out the lambda j values for each of the elements of the subset dataset
    lambdaj<-lambda[[lambdajsubset]]$Lambda
    lambdavec<-c(lambdavec,lambdaj) #this building the vector for use
  }
  for (i in 1:length(lambdavec)){
    sumunit<-(1/(lambda[[id]]$Lambda+lambdavec[i])) #This creates the summation term unit by unit with the lambda i value and all of the respective lambda js 
    sumvec<-as.vector(c(sumvec,sumunit)) #this makes a vector of the summation terms
  }
  summationterm<-sum(sumvec) #here we sum the terms of the vector to plug into the equation
  output<-(a-1+sum(dat[[id]]$Choose))/(b+summationterm) #this is where we finish up the equation and plug in all of our respective parts
  return(output)
}
#this is the old slower function
bradleyterry<-function(a,b,id,lambda,dat){
  subsetdata<-dat[dat$DocIDi %in% id,]#this subsets the dataset down to just the observations with the id that we are looking at
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
bradleyterry(a=1,b=1,id=4969,lambda,HIT2)



#### FUNCTION 2 #######

#this is the old function
bradleyterry.multid<-function(a, b, id, lambda, dat){
  output<-sapply(id, function(x) bradleyterry(a,b,id=x, lambda, dat))
  output<-cbind(id,output)
  output<-as.data.frame(output)
  colnames(output)<-c('DocId','Lambda')
  return(output)
}

#this was a dummy function for the one with subset data
bradleyterry.multide<-function(a, b, id, lambda, dat){
  output<-sapply(id, function(x) bradleyterryeasy(a,b,id=x, lambda, dat))
  output<-cbind(id,output)
  output<-as.data.frame(output)
  colnames(output)<-c('DocId','Lambda')
  newout<-lambdatrans(id,output)
  return(output)
}

#### FUNCTION 3 #######

#3 Basic function
iterative.bt<-function(a,b,id,lambda,dat, iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda<-bradleyterry.multid(a,b,id,lambda,dat) #run the code above for one doc id, a number of times determined by user
  }
  return(lambda) #returns the output as the number of iterations determined by the user.
}

#basic function with tolerance level
iterative.bt.tol<-function(a,b,id,lambda,dat,iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda1<-bradleyterry.multid(a,b,id,lambda,dat) #run the code above for one doc id, a number of times determined by user
    if (all(abs(lambda1$Lambda-lambda$Lambda)<1e-2)){
      break}
      else{
        lambda<-lambda1
      }
  }
  return(lambda1) #returns the output as the number of iterations determined by the user.
}
#================================================================================
#================================================================================
recovered0<-iterative.bt(1,1,DocId,lambda,HIT2,10)

recovered<-recovered0

cor(recovered$Lambda,apiTest$rating)
cor(recovered$Lambda,comparison$Lambda)
cor(log(recovered$Lambda),apiTest$rating)
cor(log(recovered$Lambda),comparison$Lambda)


plot(recovered$Lambda,apiTest$rating)
plot(recovered$Lambda,comparison$Lambda)
plot(log(recovered$Lambda),apiTest$rating)

#------------------------------------------------------------------------------
#Rewrites from Jacob
#we need to rename these things; change how we load the data initially ^^ for use in these functions
#------------------------------------------------------------------------------
## Re arranging data

# this is the lambda I'm updating right now
x<-5011 


rownames(lambda)<-lambda$DocId 

lambdax<-lambda[paste0(x),"Lambda"] 

rownames(lambda)<-lambda$DocId #rename each row the the DocID; the row names are strings/ characters 

lambdax<-lambda[paste0(x),"Lambda"] #get the lambda value of x=5011
#same thing as ^ doc.x.lambda<-lambda[paste0(x),"Lambda"] # the prior lambda value of doc x
lambdax
thisChoos<-HIT2[which(HIT2$DocIDi==x),"Choose"] #make a vector of "choose" of the DocIDis compared to Doc 5011
thisLambda<-lambda[paste0(HIT2[which(HIT2$DocIDi==x),"DocIDj"]),] #make a df of DocIds, lambda from HIT2; similar to lambdax
#^why do some of the row names have decimals?
newData<-cbind(thisChoos, thisLambda)# df of choose, DocId, and  lambda from HIT2
newData
#this should stay in R; rename the function and objects
dataReorganizer<-function(x){ #this is a function that does line 235-241 for a specified DocID
  lambdax<-lambda[paste0(x),"Lambda"]
  thisChoos<-HIT2[which(HIT2$DocIDi==x),"Choose"]
  thisLambda<-lambda[paste0(HIT2[which(HIT2$DocIDi==x),"DocIDj"]),]
  #
  newData<-cbind(thisChoos, thisLambda)
  newData
}

dataReorganizer(5011) #organizes the DF for x, the DocID
#this needs to be in rcpp
#updates one docId, which corresponds with lambdax
updateLambdax<-function(newData, lambdax, a=1, b=1){ #lambdax is the lmabda of DocIDx (5011); DataFrame, double, int, int
  numerator<-(a-1)+sum(newData$thisChoos)
  denominator<-(b+sum(1/(lambdax+newData$Lambda)))
  return(numerator/denominator)
}
updateLambdax(newData=newData, lambdax=lambdax)


#make this function (toPasstoC) in rcpp
library(plyr)
#toPassToC<-
toPassToC<-  lapply(unique(lambda$DocId), dataReorganizer)#makes a list of DFs where each DF is the updated lambdas for all comparisons of doc dyads
names(toPassToC)<-unique(lambda$DocId)

#this function will b in R but call on a C function; see line 253
updateLambdax2<-function(allData, lambda, thisName, a=1, b=1){ #allData is an rcpp function; thisName is a string of the DocID
  lambdax<-lambda[thisName, "Lambda"]
  newData<-allData[[thisName]]
   
  ### R will nwo call some c function right here
  ## Wil datke in lambax and newData and b and a and return numerator/denomintor
  
  numerator<-(a-1)+sum(newData$thisChoos) ## this works in c
  denominator<-(b+sum(1/(lambdax+newData$Lambda))) ## this works in c
  return(numerator/denominator) # do the division here in c 2
}
updateLambdax2(allData=toPassToC, lambda=lambda, thisName="5012")

#in rcpp
## Update all Lambdas
ob<-sapply(paste0(unique(lambda$DocId)), FUN=updateLambdax2, lambda=lambda, allData= toPassToC )
class(ob)
## Update across time and check stoping rules
## Make r code


dataReorganizer()



updateLambdax(newData=newData, lambdax=lambdax)


b=1
sum(1/(lambdax+newData$Lambda))

plot(log(recovered$Lambda),comparison$Lambda)


#this gets the estimate from Dave and Jacob
library(readr)
apiTest <- read_csv("~/Documents/GitHub/BradelyTerryModel/apiTest.csv")
apiTest$id
apiTest$rating
install.packages("rstan")
library(rstan)
setwd("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/")
load("fitExperiment2.7")
#We need to compare the output of our model to pst.lambda, the output of their stan model
post.lambda = summary(fitExperiment2.7)$summary[paste0('a[',1:50,']'),'mean']
comparison<-cbind(apiTest$id,post.lambda)

comparison<-as.data.frame(comparison)
colnames(comparison)<-c("DocId", "Lambda")
