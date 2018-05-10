###BEN!!!!!!!!!
#read in the datafrom line 5 to 48
#set your working directory to the Rcpp file so you can source the .cpp files
#jacob said he wants the only inputs of the final functions to be hits, lambda, and DocId, but I think HIT2 is useful; we should do whatever
#works best
rm(list=ls())

#Read in data
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
lambda<- as.data.frame(cbind(DocId, runif(50)))
colnames(lambda)<-c("DocIDj", "Lambda")

#source .cpp files for R function

setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/devtools.bradleyterry/Rcpp")
Rcpp::sourceCpp("posteriorlambda.cpp")
Rcpp::sourceCpp("getlambda.cpp")
Rcpp::sourceCpp("lambdaLoop2.cpp")


#--------------------

#======================================
final<- function(hits, lambdas, DocIds, iterations){
  HIT3<-merge(hits, lambdas, by="DocIDj") 
 for( i in 1:iterations){ #change arguments of lambdas
    lambda1<-lambdaLoop2(hits=HIT2, DocIds = DocId,Hit3 = HIT3, extractLambda=lambdas$Lambda)
    lambda1<-lambda1[-1]
    #lambdas$Lambda<-lambda1[-1]
    
    if (all(abs(lambda1-lambdas$Lambda)<1e-15)){
      break
      }
    else{
      lambdas$Lambda<-lambda1
    }
 }
  return(lambda1)

}
final(HIT2, lambda, DocId, 1000)
library(microbenchmark)
microbenchmark(final(HIT2, lambda, DocId, 1000))
