context("Correct estimation of Lambda for one Doc ID")
#CREATE FAKE DATA
toydata<-data.frame(sort(rep(seq(1,10),10)), sample(1:10, 100, replace=T), sample(c(0,1), 100, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
toydata
table(toydata$DocIDj)

#Remove rows where doci=docj
toydata$delete<- (toydata$DocIDi==toydata$DocIDj)
toydata2<-subset(toydata,delete=="FALSE")
toydata2<-toydata2[,-4]
dataset<-toydata2
#create lambda data frame
lambda<-data.frame(c(10:1),.5)
colnames(lambda)<-c('DocId', 'Lambda')


test_that("Scalar lambda output",{
  expect_that(bradleyterry(1,1,10,lambda,dataset), equals(4/11))
})
lambda<-data.frame(c(1:10),runif(10))
colnames(lambda)<-c('DocId', 'Lambda')