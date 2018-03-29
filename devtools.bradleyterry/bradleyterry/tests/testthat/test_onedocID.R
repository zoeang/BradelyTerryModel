context("Correct estimation of Lambda for one Doc ID")
#CREATE FAKE DATA
docjdat<-c(2,seq(2,10,1), 1, seq(3,10,1), 1,2, seq(4,10,1),1, seq(1,3,1), seq(5,10,1),
           1, seq(1,4,1), seq(6,10,1),
           1,3,seq(1,5,1), seq(7,10,1),
           1, 3,seq(1,6,1), seq(8,10,1),
           1, 3,seq(1,7,1), seq(9,10,1),
           1, 3, seq(1,9,1),1, seq(2,8,1))
rep(c(1,0,1,1,0),20)
dataset<-toydata<-data.frame(sort(rep(seq(1,10),10)), docjdat, rep(c(1,0,1,1,0),20))

colnames(dataset)<-c("DocIDi", "DocIDj", "Choose")

#create lambda data frame
lambda<-data.frame(c(10:1),.5)
colnames(lambda)<-c('DocId', 'Lambda')

#more fake data======================================
docjdat2<-c(seq(2,11,1), seq(3,12,1), seq(4,13,1), seq(5,14,1), seq(6,15,1), seq(7,16,1), seq(8,17,1), seq(9,18,1), seq(10,19,1), seq(9,0,-1))
dataset2<-data.frame(sort(rep(seq(1,10),10)), docjdat2, rep(c(0,1,0,1,1),20))
colnames(dataset2)<-c("DocIDi", "DocIDj", "Choose")
lambda2<-data.frame(c(10:1),c(.25,.5,.3,.1,.8,.55,.9,0,.75, .1))
colnames(lambda2)<-c('DocId', 'Lambda')

#This is the test==================================================
test_that("Scalar lambda output",{
  expect_that(bradleyterry(1,1,10,lambda,dataset), equals(6/11))
  expect_that(bradleyterry(1,1,10,lambda2,dataset2), equals(6/11))
})
bradleyterry(1,1,id=5,lambda2,dataset2)
