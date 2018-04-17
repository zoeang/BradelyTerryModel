dat<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel/CombinedOutputExperiment2.csv", header = T)
dat2<-dat[,3:5]
head(dat2)
sort(unique(dat2$document_id), decreasing = F)
