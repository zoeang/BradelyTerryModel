library(devtools)
library(roxygen2)

setwd("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/devtools.bradleyterry")
current.code<-as.package("bradleyterry")
load_all(current.code)
document(current.code)


toydata<-data.frame(rep(1,30), sample(1:100, 30, replace=T), sample(c(0,1), 30, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
lambdai<-runif(30)
lambdaj<-runif(30)


lambdai<-as.vector(c(.25,.25))
lambdaj<-as.vector(c(.25,.25))

toydata<-data.frame(rep(1,2), sample(1:100, 2, replace=T), sample(c(0,1), 2, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
toydata
dataset<-toydata

bradleyterry(1,1,lambdai,lambdaj,toydata)

