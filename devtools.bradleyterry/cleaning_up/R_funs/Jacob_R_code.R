
#------------------------------------------------------------------------------
#Rewrites from Jacob
#we need to rename these things; change how we load the data initially ^^ for use in these functions
#------------------------------------------------------------------------------
## Re arranging data

# this is the lambda I'm updating right now
x<-5011  #if you want to change doc id (for instance, 4969), just change all 5011 to 4969.


rownames(lambda)<-lambda$DocId 

lambdax<-lambda[paste0(x),"Lambda"] 

rownames(lambda)<-lambda$DocId #rename each row the the DocID; the row names are strings/ characters 

lambdax<-lambda[paste0(x),"Lambda"] #get the lambda value of x=5011
#same thing as ^ doc.x.lambda<-lambda[paste0(x),"Lambda"] # the prior lambda value of doc x
lambdax
thisChoos<-HIT2[which(HIT2$DocIDi==x),"Choose"] #make a vector of "choose" of the DocIDis compared to Doc 5011
thisLambda<-lambda[paste0(HIT2[which(HIT2$DocIDi==x),"DocIDj"]),] #make a df of DocIds, lambda from HIT2; similar to lambdax
#^why do some of the row names have decimals?
newData<-cbind(thisChoos, thisLambda)# df of choose, DocId, and  lambda from HIT2
newData
#this should stay in R; rename the function and objects
dataReorganizer<-function(x){ #this is a function that does line 235-241 for a specified DocID
  lambdax<-lambda[paste0(x),"Lambda"]
  thisChoos<-HIT2[which(HIT2$DocIDi==x),"Choose"]
  thisLambda<-lambda[paste0(HIT2[which(HIT2$DocIDi==x),"DocIDj"]),]
  #
  newData<-cbind(thisChoos, thisLambda)
  newData
}

dataReorganizer(5011) #organizes the DF for x, the DocID
#this needs to be in rcpp
#updates one docId, which corresponds with lambdax
updateLambdax<-function(newData, lambdax, a=1, b=1){ #lambdax is the lmabda of DocIDx (5011); DataFrame, double, int, int
  numerator<-(a-1)+sum(newData$thisChoos)
  denominator<-(b+sum(1/(lambdax+newData$Lambda)))
  return(numerator/denominator)
}
updateLambdax(newData=newData, lambdax=lambdax)


#make this function (toPasstoC) in rcpp
library(plyr)
#toPassToC<-
toPassToC<-  lapply(unique(lambda$DocId), dataReorganizer)#makes a list of DFs where each DF is the updated lambdas for all comparisons of doc dyads
names(toPassToC)<-unique(lambda$DocId)

#this function will b in R but call on a C function; see line 253
updateLambdax2<-function(allData, lambda, thisName, a=1, b=1){ #allData is an rcpp function; thisName is a string of the DocID
  lambdax<-lambda[thisName, "Lambda"]
  newData<-allData[[thisName]]
  
  ### R will nwo call some c function right here
  ## Wil datke in lambax and newData and b and a and return numerator/denomintor
  
  numerator<-(a-1)+sum(newData$thisChoos) ## this works in c
  denominator<-(b+sum(1/(lambdax+newData$Lambda))) ## this works in c
  return(numerator/denominator) # do the division here in c 2
}
updateLambdax2(allData=toPassToC, lambda=lambda, thisName="5012")

#in rcpp
## Update all Lambdas
ob<-sapply(paste0(unique(lambda$DocId)), FUN=updateLambdax2, lambda=lambda, allData= toPassToC )
class(ob)
## Update across time and check stoping rules
## Make r code


dataReorganizer()



updateLambdax(newData=newData, lambdax=lambdax)


b=1
sum(1/(lambdax+newData$Lambda))

plot(log(recovered$Lambda),comparison$Lambda)

