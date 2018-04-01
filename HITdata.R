HIT<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/exampleHITs.csv", header=T)
HIT
lambdaDF<-data.frame(HIT$document_id[1:100],round(runif(100), 3)) 
colnames(lambdaDF)<-(c("DocId", "Lambda"))
lambdaDF
