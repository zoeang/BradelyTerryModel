###################################################################
#The functions in here are basic R bradleyterry functions with different versions and
#Do similar work but different purposes.
###################################################################
#This function uses the pre-formatted 'lambda' and 'dataset': use for rcpp 
bradleyterryeasy<-function(a,b,id,lambda,dat){
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in 1:nrow(dat[[id]])){ #the purpose of thsi loop is to extract lambda j values for use in the next loop
    lambdajsubset<-dat[[id]]$DocIDj[i] #This picks out the lambda j values for each of the elements of the subset dataset
    lambdaj<-lambda[[lambdajsubset]]$Lambda
    lambdavec<-c(lambdavec,lambdaj) #this building the vector for use
  }
  for (i in 1:length(lambdavec)){
    sumunit<-(1/(lambda[[id]]$Lambda+lambdavec[i])) #This creates the summation term unit by unit with the lambda i value and all of the respective lambda js 
    sumvec<-as.vector(c(sumvec,sumunit)) #this makes a vector of the summation terms
  }
  summationterm<-sum(sumvec) #here we sum the terms of the vector to plug into the equation
  output<-(a-1+sum(dat[[id]]$Choose))/(b+summationterm) #this is where we finish up the equation and plug in all of our respective parts
  return(output)
}

#this is the old slower function
bradleyterry<-function(a,b,id,lambda,dat){
  subsetdata<-dat[dat$DocIDi %in% id,]#this subsets the dataset down to just the observations with the id that we are looking at
  newlambda<-lambda[lambda$DocId %in% id,]#this extracts the specific DocID and lambda value we want to upgrade for the purpose of the equation 
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in 1:nrow(subsetdata)){ #the purpose of thsi loop is to extract lambda j values for use in the next loop
    lambdajsubset<-lambda[lambda$DocId %in% subsetdata$DocIDj[i],] #This picks out the lambda j values for each of the elements of the subset dataset
    lambdavec<-c(lambdavec,lambdajsubset$Lambda) #this building the vector for use
  }
  for (i in 1:nrow(subsetdata)){
    sumunit<-(1/(newlambda$Lambda+lambdavec[i])) #This creates the summation term unit by unit with the lambda i value and all of the respective lambda js 
    sumvec<-as.vector(c(sumvec,sumunit)) #this makes a vector of the summation terms
  }
  summationterm<-sum(sumvec) #here we sum the terms of the vector to plug into the equation
  output<-(a-1+sum(subsetdata$Choose))/(b+summationterm) #this is where we finish up the equation and plug in all of our respective parts
  return(output)
}



#### FUNCTION 2 #######

#this is the old function
bradleyterry.multid<-function(a, b, id, lambda, dat){
  output<-sapply(id, function(x) bradleyterry(a,b,id=x, lambda, dat))
  output<-cbind(id,output)
  output<-as.data.frame(output)
  colnames(output)<-c('DocId','Lambda')
  return(output)
}

#this was a dummy function for the one with subset data
bradleyterry.multide<-function(a, b, id, lambda, dat){
  output<-sapply(id, function(x) bradleyterryeasy(a,b,id=x, lambda, dat))
  output<-cbind(id,output)
  output<-as.data.frame(output)
  colnames(output)<-c('DocId','Lambda')
  newout<-lambdatrans(id,output)
  return(output)
}


#### FUNCTION 3 #######

#3 Basic function
iterative.bt<-function(a,b,id,lambda,dat, iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda<-bradleyterry.multid(a,b,id,lambda,dat) #run the code above for one doc id, a number of times determined by user
  }
  return(lambda) #returns the output as the number of iterations determined by the user.
}

#basic function with tolerance level
iterative.bt.tol<-function(a,b,id,lambda,dat,iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda1<-bradleyterry.multid(a,b,id,lambda,dat) #run the code above for one doc id, a number of times determined by user
    if (all(abs(lambda1$Lambda-lambda$Lambda)<1e-3)){
      break}
    else{
      lambda<-lambda1
    }
  }
  return(lambda1) #returns the output as the number of iterations determined by the user.
}
