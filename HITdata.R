#Note: the HIT data may not have enough comparisons of each ID to accurately estimate
#lambda over multiple iterations (not enough comparisons to converge)
#==============================================================================
#Read in Data
#==============================================================================
#Zoe
HIT<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/exampleHITs.csv", header=T)
#Ben
HIT<-read.csv("/Users/benjaminschneider/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
#Lim
HIT<-read.csv("C:/Users/dell/Documents/GitHub/BradelyTerryModel/exampleHITs.csv", header=T)
colnames(HIT)<-c("DocIDi", "DocIDj", "Choose")
#==============================================================================
#Create a lambda dataframe where each unique DocID (i and j) has a lambda
#==============================================================================
uniqueDocID<-c(unique(HIT[,1]),unique(HIT[,2]))
set.seed(13)
lambda.value<-round(runif(uniqueDocID),3)
lambda.df<-data.frame(uniqueDocID, lambda.value)
colnames(lambda.df)<-c('DocId', 'Lambda')
head(lambda.df) #check it out

#==============================================================================
#Meta-HIT: create a DF where DocIDi and DocIDj are combined by roww, not column
#and change the "Choose" outcome for rows with DocIDJ (imply that DocIDj was chosen)
#==============================================================================
for(i in 1:nrow(HIT)){ #this loop will change the "Choose" column for
if(HIT$Choose[i]==0){  #comparing DocIDj's to DocIDi's
  HIT$Choose2[i]=1
} else if(HIT$Choose[i]==1){
  HIT$Choose2[i]=0
}
}
sum(HIT$Choose==HIT$Choose2) #test that loop worked: should equal 0
#Flip the comparison between Docs i and j======================================
HIT2<-as.data.frame(cbind(HIT$DocIDj, HIT$DocIDi, HIT$Choose2)) #HIT where j is compared to i
colnames(HIT2)<-c("DocIDi", "DocIDj", "Choose")
head(HIT2)# take a look
HIT<-HIT[,-4] #remove the "Choose2" column

metaHIT<-rbind(HIT, HIT2)
head(metaHIT) #compare
metaHIT[c(501:506), ] #compare

metaHIT #this is the dataset
#^metaHIT dataset complete=====================================================
#==============================================================================


#making new hit data

id<-unique(HIT$DocIDi) 

lambda<-data.frame(c(10:1),runif(10))
colnames(lambda)<-c('DocId', 'Lambda')
HIT$lambda<-runif(nrow(HIT))
head(HIT)

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

