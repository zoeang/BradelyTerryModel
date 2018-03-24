toydata<-data.frame(rep(1,30), sample(1:100, 30, replace=T), sample(c(0,1), 30, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
toydata
table(toydata$DocIDj)

