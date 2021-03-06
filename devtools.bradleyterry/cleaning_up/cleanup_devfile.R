rm(list=ls())
library(devtools)
library(roxygen2)

###### In R only. Old code ########
###################################
#Data Generation===============================================================
dat<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)

HIT<-dat[,3:5]
setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs")
setwd('C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs')
source("datatransform.R") #use only once

#We will use HIT2 for R only functions in most cases
HIT2<-datatransform(HIT)

#For testing our old R function
set.seed(42)
DocId<-unique(HIT$document_id)
DocId<-sort(DocId, decreasing = F)
lambda<-cbind(DocId,runif(50))
lambda<-as.data.frame(lambda)
colnames(lambda)<-c("DocId", "Lambda")
#Here is our old function which is a baseline 
#this is the old slower function
setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs")

source("com_bradley.R")
#Give updated lambda: just one lambda for one id
bradleyterry(a=1,b=1,id=4969,lambda,HIT2)

#Give updated lambdas: just multi lambda for multi ids
DocId
bradleyterry.multid(a=1,b=1, id=DocId, lambda, HIT2)

iterative.bt(a=1,b=1,DocId,lambda,dat=HIT2,iterations=3)

#tolerance test #
iterative.bt.tol(a=1,b=1,DocId,lambda,dat=HIT2,iterations=100)

#Load Jacob and David's lambdas
library(readr)
apiTest <- read_csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/apiTest.csv")
apiTest<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/apiTest.csv")
apiTest$id
apiTest$rating
#install.packages("rstan")
library(rstan)
setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/")
setwd("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/")
setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel")
load("fitExperiment2.7")

#We need to compare the output of our model to pst.lambda, the output of their stan model
post.lambda = summary(fitExperiment2.7)$summary[paste0('a[',1:50,']'),'mean']
comparison<-cbind(apiTest$id,post.lambda)
comparison<-as.data.frame(comparison)
colnames(comparison)<-c("DocId", "Lambda")

#Last thing: see how this works 
#guide line for number of iterations:1000 and tolerance is 1e-8
recovered<-iterative.bt.tol(1,1,DocId,lambda,HIT2,100)

cor(recovered$Lambda,apiTest$rating)
cor(recovered$Lambda,comparison$Lambda)
cor(log(recovered$Lambda),apiTest$rating)
cor(log(recovered$Lambda),comparison$Lambda)
plot(comparison$Lambda,log(tolOut1 ))
##############################################
#This is what we need to check finally########
##############################################
plot(recovered$Lambda,apiTest$rating)
plot(recovered$Lambda,comparison$Lambda)
plot(log(recovered$Lambda),apiTest$rating)
#this outcome will be better if I make tolerance higher and increase iterations.
#checked before.

#Rcpp application
#Prepare the data: With RCPP we will use HIT3 data for a while
dat<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)

HIT<-dat[,3:5]
setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs")
setwd('C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs')
source("datatransform.R") #use only once

#We will use HIT2 for R only functions in most cases
HIT2<-datatransform(HIT)

#For testing our old R function
set.seed(42)
DocId<-unique(HIT$document_id)
DocId<-sort(DocId, decreasing = F)
Lambda<-runif(50)
lambda<-cbind(DocId,Lambda)
lambda<-as.data.frame(lambda)
#colnames(lambda)<-c("DocIDj", "Lambda")

library(plyr)
#Why are we doing this?
HIT2$Lambda <- lambda$Lambda[match(HIT2$DocIDj, lambda$DocId)]
#Below will be the same as above
#colnames(lambda)<-c("DocIDj","Lambda")
#HIT2.1<-merge(HIT2, lambda, by="DocIDj")

setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry/Rcpp")
setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry/Rcpp")

Rcpp::sourceCpp("posteriorlambda.cpp")
Rcpp::sourceCpp("getlambda.cpp") 
Rcpp::sourceCpp("lambdaLoop2.cpp")



#bring "newData" from jacob_R_code.
newData
newnew<-as.data.frame(cbind(newData$thisChoos,newData$DocId,newData$Lambda))
colnames(newnew)<-c("Choose","DocId","Lambda") #rename it for postlambda function.

posteriorlambda(newnew, lambda$Lambda[1], 1,1) #one updated lambda for id 4969
#You can see this rcpp function's outcome is same as the outocme of bradleyterry in R.

lambda2<-lambda
colnames(lambda2)<-c("DocId","Lambda") #same as lambda, but because of the name of a column, I need to do this.
bradleyterry(a=1,b=1,id=4969,lambda2,HIT2)

#this is same as "bradleyterry.multid" in R.
lambdaLoop2(hits=HIT2,DocIds = DocId,Hit3 = HIT2, extractLambda=lambda$Lambda)

#we need to take out the first value becasue of indexing problem.
out<-lambdaLoop2(hits=HIT2,DocIds = DocId, Hit3 = HIT2, extractLambda=lambda$Lambda)
out<-out[-1] #check

#Same as "lambdaLoop2", but the outcome is not vector
allUpdatedLambda(hits=HIT2, lambdas=lambda, DocIds=DocId)

setwd("C:/Users/dell/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs")
setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry/cleaning_up/R_funs")
source("tolTest.R")

#Rccp tolerance test.
tolTest(HIT2, lambda, DocId,100)

#Test the speed: "Rcpp" vs "R only"
library(microbenchmark)
microbenchmark(tolTest(HIT2, lambda, DocId, 10))
microbenchmark(iterative.bt.tol(1,1,DocId,lambda,HIT2,10))

#btw this is the benchmark outcome
#>microbenchmark(tolTest(HIT2, lambda, DocId, 10))
#Unit: milliseconds
#expr      min       lq     mean   median       uq
#final(HIT2, lambda, DocId, 10) 220.1306 229.1866 257.7195 239.9635 270.7927
#max neval
#434.0704   100
#> microbenchmark(iterative.bt.tol(1,1,DocId,lambda,HIT2,10))
#Unit: seconds
#expr      min       lq     mean
#iterative.bt.tol(1, 1, DocId, lambda, HIT2, 10) 2.209509 2.410866 2.758498
#median       uq      max neval
#2.584111 3.009335 4.087998   100
##############################################
#This is what we need to check finally########
##############################################
Rcpp_out<-tolTest(HIT2, lambda, DocId,10000 )

tolTest(HIT2, lambda, DocId,200 )

#Test the speed: "Rcpp" vs "R only"
library(microbenchmark)
microbenchmark(tolTest(HIT2, lambda, DocId, 100), iterative.bt.tol(1,1,DocId,lambda,HIT2,100))

##############################################
#This is what we need to check finally########
##############################################
Rcpp_out<-tolTest(HIT2, lambda, DocId,200 )
comparison<-as.data.frame(comparison)
colnames(comparison)<-c("DocId", "Lambda")
plot(Rcpp_out, comparison$Lambda)
plot(log(Rcpp_out), comparison$Lambda)
cor(log(Rcpp_out), comparison$Lambda) 




