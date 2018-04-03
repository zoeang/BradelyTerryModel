HIT<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
#Create a lambda dataframe where each DocIDi has a lambda; unique DocIDj will have a lambda value; there should be four columns
lambdaDF<-data.frame(HIT$DocIDi[1:100],HIT$DocIDj[1:100]) 
uniqueDocID<-c(unique(lambdaDF[,2]),unique(lambdaDF[,1]))
set.seed(13)
lambdaDocIDj<-round(runif(uniqueDocID),3)
lambda<-data.frame(uniqueDocID, lambdaDocIDj)
colnames(lambda)<-c('DocId', 'Lambda')
#lambda dataframe
lambda
table(HIT$DocIDj)
