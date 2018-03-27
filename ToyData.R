#CREATE FAKE DATA
toydata<-data.frame(sort(rep(seq(1,10),10)), sample(1:10, 100, replace=T), sample(c(0,1), 100, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
toydata
table(toydata$DocIDj)

#Remove rows where doci=docj
toydata$delete<- (toydata$DocIDi==toydata$DocIDj)
toydata2<-subset(toydata,delete=="FALSE")
toydata2<-toydata2[,-4]
toydata2
