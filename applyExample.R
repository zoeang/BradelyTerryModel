library(plyr)
ddply(toydata2,2)
#ply example from Lecture 8====================================================
dd<-data.frame(matrix(rnorm(216),72,3),c(rep("A",24),rep("B",24),rep("C",24)),c(rep("J",36),rep("K",36)))
colnames(dd) <- c("v1", "v2", "v3", "dim1", "dim2")
head(dd)
dd
obj1 <- ddply(.data=dd, .variables=c("dim1","dim2"), .fun=function(df) mean(df$v1))
obj1
#maybe?
ddply(.data=dataset, .variables=c("DocIDi", "DocIDj"), .fun=bradleyterry(a,b,id,lambda,dataset))
#end example===================================================================