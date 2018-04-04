HIT<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/exampleHITs.csv", header=T)
HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
#Create a lambda dataframe where each DocIDi has a lambda; unique DocIDj will have a lambda value; there should be four columns
lambdaDF<-data.frame(HIT$DocIDi[1:100],HIT$DocIDj[1:100]) 
uniqueDocID<-c(unique(lambdaDF[,2]),unique(lambdaDF[,1]))
set.seed(13)
lambdaDocIDj<-round(runif(uniqueDocID),3)
lambda<-data.frame(uniqueDocID, lambdaDocIDj)
colnames(lambda)<-c('DocId', 'Lambda')
#lambda dataframe


#Meta-HIT
for(i in 1:nrow(HIT)){
if(HIT$Choose[i]==0){
  HIT$Choose2[i]=1
} else if(HIT$Choose[i]==1){
  HIT$Choose2[i]=0
}
}

HIT2<-as.data.frame(cbind(HIT$DocIDj, HIT$DocIDi, HIT$Choose2))
HIT<-HIT[,-4]
colnames(HIT2)<-c("DocIDi", "DocIDj", "Choose")
head(HIT,2)
#===========================
metaHIT<-rbind(HIT, HIT2)

