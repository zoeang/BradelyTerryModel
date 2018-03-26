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

bradleyterry(1,1,lambdai,lambdaj,toydata)

#hi
