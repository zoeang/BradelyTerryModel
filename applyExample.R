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
ddply(.data=dataset, .variables=c("id"), .fun=bradleyterry(1,0,id=3,lambda,dataset))
#end example===================================================================

#will apply work? The .variables argument is the column of the dataframe to iterate over,
#but we want to interate over an argument of the function(id), not a dataset column
head(dataset)
sapply(unique(dataset$DocIDi), function(x) bradleyterry(1,0,id=x, lambda, dataset))
bradleyterry.multid.apply<-function(a, b, id, lambda, dataset){
  sapply(id, function(x) bradleyterry(a,b,id=x, lambda, dataset))
}

sapply(unique(dataset$DocIDi), function(x) paste(x, bradleyterry(1,0,id=x, lambda, dataset)))


