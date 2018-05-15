rm(list=ls())




#read in data
dat<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
HIT<-dat[,3:5]

#data transformation; allows us to calculate posterior lambdas for DocIs and DocJs; this only needs to be run once, so I'm not rewriting it
datatransform<-function(HIT){
  vec<-rep(1:2, 1500)
  HIT<-as.data.frame(cbind(HIT,vec))
  colnames(HIT)<-c("comparison_id" ,"document_id", "result","num")
  DocIDi<-NULL
  DocIDj<-NULL
  Choose<-NULL
  for (i in 1:nrow(HIT)){ #create a vector of DocIDj composed of every other document_id, the document to which the document of interest of compared
    if (HIT$num[i]==2){
      DocIDj<-c(DocIDj,HIT$document_id[i])
    }
    else{
      DocIDi<-c(DocIDi,HIT$document_id[i]) #create a vector of DocIDi composed every other document id, the document of interest 
      Choose<-c(Choose,HIT$result[i]) #1 indicates that the document of interest won; 0 indicates that the document of interest lost
    }
  }
  newHIT<-as.data.frame(cbind(DocIDi,DocIDj,Choose))
  DocIDi<-NULL #repeat the same process, but reverse the order of comparison; this ultimately allows us to update all lambdas
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

DocId<-sort(unique(HIT$document_id), decreasing=F)
set.seed(42)
lambda<- as.data.frame(cbind(DocId, runif(50)))
colnames(lambda)<-c("DocIDj", "Lambda")

########################## STOP HERE




#create a dataframe of lambdas; the lambdas should converge better if they are not all the same value according to Jacob
DocId<-sort(unique(HIT$document_id), decreasing=F)
lambda<- as.data.frame(cbind(DocId, runif(50))) #lambda values are random 
rownames(lambda)<-lambda$DocId #allows the row to be called using x in the next line
docXprior<-lambda[paste0(x),"Lambda"]# the prior lambda value of doc x
#This function will create a dataframe for one document, where the first column indicates if the
#document won (1) or lost (0); the second column is the document to which the dataframe document
#was compared; and the third column is the prior probability that the datafram document "wins"




#VERIFY: the function will search in the environment for 'lambda' and 'HIT2'
doc.x.dataframe<-function(x){#x is the document ID for which the dataframe will be made
  x.choose<-HIT2[which(HIT2$DocIDi==x),"Choose"] #a vector indicating if doc x was chosen 
  thisLambda<-lambda[paste0(HIT2[which(HIT2$DocIDi==x),"DocIDj"]),] #dataframe of documents to which x was compared and the prior lambda of x winning for each doc
  newData<-cbind(x.choose, thisLambda)
}


#calculate the prior lambda for document x; this will be an argument in the Rcpp function to calculate the posterior lambda
#define x, the document of interest
rownames(lambda)<-lambda$DocId #allows the row to be called using x in the next line
docXprior<-lambda[paste0(x),"Lambda"]# the prior lambda value of doc x



###See posteriorlambda.cpp

###See postLambdaAllDocs.cpp
names(postLambdaAllDocs)<-unique(lambda$DocId)

postLambdaX<-function(listAllData, lambda, docString, a=1, b=1){
  docXprior<-lambda[docString, 'Lambda']
  newData<-listAllData[[docString]]
  #call rcpp function 'posteriorlambda'
}

######################################### End hold

DocId<-sort(unique(HIT$document_id), decreasing=F)
HIT2<-datatransform(HIT)
set.seed(42)
lambda<- as.data.frame(cbind(DocId, runif(50))) #lambda values are random 
colnames(lambda)<-c("DocIDj", "Lambda")
HIT3<- merge(HIT2, lambda, by="DocIDj")


setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry/Rcpp")
Rcpp::sourceCpp("posteriorlambda.cpp")
Rcpp::sourceCpp("getlambda.cpp")
#Rcpp::sourceCpp("subsetLambdasDF.cpp")
Rcpp::sourceCpp("lambdaLoop2.cpp")

getlambda(lambda, DocId)
posteriorlambda(HIT3, lambda$Lambda[1], 1,1)
lambda[1,]

lambdaLoop2(hits=HIT2,DocIds = DocId,Hit3 = HIT3, extractLambda=lambda$Lambda)
#subsetlambdas(lambdas=lambda, DocIds=DocId)
#rely on line 7 to 46 and 100; nothing else should be found in the environment
#make this into a function; out is lambda; arguments are DocId, HIT2, lambda
allUpdatedLambda<-function(hits, lambdas, DocIds){
  HIT3<-merge(hits, lambdas, by="DocIDj") #this is an r function
  # below is a c++ function that calls the c++ function posteriorlambda
  for(i in 1:length(DocIds)){
    x<-DocIds[i]
    newData<-HIT3[which(hits$DocIDi==x), c("Choose", "Lambda", "DocIDj")] #nest an rcpp loop in an r loop?
    lambdas[lambdas$DocIDj==DocIds[i],2]<-posteriorlambda(newData,lambdas[lambdas$DocIDj==DocIds[i],2], a=1, b=1)
  }
  return(lambdas)
}
head(HIT2)
head(HIT3)
colnames(lambda)<-c("DocIDj", "Lambda")
HIT3=merge(HIT2, lambda, by="DocIDj")
allUpdatedLambda(hits=HIT2, lambdas=lambda, DocIds=DocId)
lambda[lambda$DocIDj==DocId[1],]
length(lambda["DocIDj"])
#everything else should be in r
#use iterative.bt.tol


