HIT<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/exampleHITs.csv", header=T)
HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
#Create a lambda dataframe where each DocIDi has a lambda; unique DocIDj will have a lambda value; there should be four columns
lambdaDF<-data.frame(HIT$DocIDi,HIT$DocIDj) 
uniqueDocID<-c(unique(lambdaDF[,2]),unique(lambdaDF[,1]))
set.seed(13)
lambdaDocIDj<-round(runif(uniqueDocID),3)
lambda<-data.frame(uniqueDocID, lambdaDocIDj)
colnames(lambda)<-c('DocId', 'Lambda')
#lambda dataframe
lambda
table(HIT$DocIDj)
id<-lambda$DocId

head(HIT,50)

which(HIT$DocIDi==1755 & HIT$DocIDj==5059)

#Meta-HIT
for(i in 1:nrow(HIT)){
if(HIT$Choose[i]==0){
  HIT$Choose2[i]=1
} else if(HIT$Choose[i]==1){
  HIT$Choose2[i]=0
}
}

sum(HIT$Choose==HIT$Choose2)
HIT2<-as.data.frame(cbind(HIT$DocIDj, HIT$DocIDi, HIT$Choose2))
HIT<-HIT[,-4]
colnames(HIT2)<-c("DocIDi", "DocIDj", "Choose")
head(HIT,2)
#===========================
metaHIT<-rbind(HIT, HIT2)
head(HIT)
head(HIT2)


#HERE WE GO.
newHIT<-as.data.frame(cbind(HIT$DocIDi,HIT$DocIDj,HIT$chosen))
colnames(newHIT)<-c("DocIDi", "DocIDj", "chosen")
#Meta-HIT
for(i in 1:nrow(newHIT)){
  if(newHIT$chosen[i]==0){
    newHIT$chosen2[i]=1
  } else if(HIT$chosen[i]==1){
    newHIT$chosen2[i]=0
  }
}
sum(newHIT$chosen==newHIT$chosen2)
newHIT2<-as.data.frame(cbind(newHIT$DocIDj, newHIT$DocIDi, newHIT$Choose2))
newHIT2<-newHIT[,-4]
colnames(newHIT2)<-c("DocIDi", "DocIDj", "chosen")
head(newHIT,2)

newHIT<-subset(newHIT, select = -c(chosen2) )
#===========================
metaHIT<-rbind(newHIT, newHIT2)
head(newHIT)
head(newHIT)

#making new hit data
HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
HIT<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
HIT

id<-unique(HIT$DocIDi) 

lambda<-data.frame(c(10:1),runif(10))
colnames(lambda)<-c('DocId', 'Lambda')
HIT$lambda<-runif(nrow(HIT))
up_lambda<-bradleyterry(1,2,1922,lambda,HIT)
newlambda<-lambda[lambda$DocId %in% id,]

#Dataset to recover lambda############
HIT #start with HIT data
HIT$lambda_i<-rep(runif(50), each=2) #create 100 random values for lambda_i

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
}
HIT

