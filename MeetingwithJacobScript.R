#### This is for the meeting with Jacob ####

#here we are creating our "true lambdas"
id<-1:10
Lam<-runif(10)
lambda<-as.data.frame(cbind(id,Lam))
colnames(lambda)<-c('DocId', 'Lambda')

#these lambdas are just dummy values to plug in, they can be literally anything other than 0s
lam<-runif(10)
lambda1<-as.data.frame(cbind(id,lam))
colnames(lambda1)<-c('DocId', 'Lambda')

data.generation<-function(lambda,n){ #n size dataset
  output.lambda<-NULL #creates a template for the output dataset
  for (i in 1:n){#this is a for loop for creating data points, n size dataset
    lams<-sample(lambda$DocId, 2)#this randomly selects two of the lambdas
    lambdavec<-NULL #creates a template for extracting the lambda values
    for (i in lams){
      lambdavec<-c(lambdavec,lambda$Lambda[i]) #this actually extracts the lambdas
    }
    prob<-lambdavec[1]/(lambdavec[1]+lambdavec[2]) #this uses the lambdas in order to create a probability for selection
    Choose <- sample(c(0,1), 1, replace = TRUE, prob = c(1-prob, prob))#this chooses the document with the predetermined probability
    new.lambda<-cbind(lams[1],lams[2],Choose) #now building our output
    output.lambda<-rbind(output.lambda, new.lambda)#row binding with the template
  }
  rownames(output.lambda)<-NULL #getting rid of the numbers for row name
  output.lambda<-as.data.frame(output.lambda) #we do this because removing our row names made a sort of matrix
  colnames(output.lambda)<-c("DocIDi","DocIDj","Choose") #Making the output like our dataset
  return(output.lambda) #outputs our data
}

#now we are going to create our dataset to run into our functions below
dataset<-data.generation(lambda,5000)

#==============================================================================
#==============================================================================

#### FUNCTION 1 #######
bradleyterry<-function(a,b,id,lambda,dataset){
  subsetdata<-dataset[dataset$DocIDi %in% id,]#this subsets the dataset down to just the observations with the id that we are looking at
  newlambda<-lambda[lambda$DocId %in% id,]#this extracts the specific DocID and lambda value we want to upgrade for the purpose of the equation 
  sumvec<-NULL #create null vectors to store our sum elements
  lambdavec<-NULL #create null vector to extract the lambda elements we want
  for(i in 1:nrow(subsetdata)){ #the purpose of thsi loop is to extract lambda j values for use in the next loop
    lambdajsubset<-lambda[lambda$DocId %in% subsetdata$DocIDj[i],] #This picks out the lambda j values for each of the elements of the subset dataset
    lambdavec<-c(lambdavec,lambdajsubset$Lambda) #this building the vector for use
  }
  for (i in 1:nrow(subsetdata)){#this is creating the summartion vector with all of our summands
    sumunit<-(1/(newlambda$Lambda+lambdavec[i])) #This creates the summation term unit by unit with the lambda i value and all of the respective lambda js 
    sumvec<-as.vector(c(sumvec,sumunit)) #this makes a vector of the summation terms
  }
  summationterm<-sum(sumvec) #here we sum the terms of the vector to plug into the equation
  output<-(a-1+sum(subsetdata$Choose))/(b+summationterm) #this is where we finish up the equation and plug in all of our respective parts
  return(output)
}


##### Function 2 #####

bradleyterry.multid.apply<-function(a, b, id, lambda, dataset){
  output<-sapply(id, function(x) bradleyterry(a,b,id=x, lambda, dataset)) #this uses an apply function to run the bradley terry model for all of our document ids
  output<-as.data.frame(cbind(id,output)) #this is just reformating the data for later iterations
  colnames(output)<-c('DocId','Lambda') #this is just reformating the data for later iterations
  return(output)
}

#### Function 3 ####

#with the code below, We can easily make an additional function with benchmarks and tolerances for the package

iterative.bt.with.benchmark<-function(a,b,id,lambda,dataset,iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda1<-bradleyterry.multid.apply(a,b,id,lambda,dataset) #run the code above for one doc id, a number of times determined by user
    if (all(abs(lambda1$Lambda-lambda$Lambda)<1e-8)){#this is our tolerance test, once every lambda is less than this difference, the loop stops
      break}
      else{
        lambda<-lambda1 #this is just reformating the data because we are doing a recursive loop
      }
  }
  return(lambda1) #returns the output as the number of iterations determined by the user.
}


iterative.bt.with.benchmark<-function(a,b,id,lambda,dataset,iterations){
  for (i in 1:iterations){   # from 1 to number of iteration, the loop repeats below function
    lambda1<-bradleyterry.multid.apply(a,b,id,lambda,dataset) #run the code above for one doc id, a number of times determined by user
    if (all(abs(lambda1$Lambda-lambda$Lambda)<1e-8)){ #this is our tolerance test, once every lambda is less than this difference, the loop stops
      break}
    else{
      if (i%%25==0){ #the purpose of this part of the function is to print out a benchmark every 25 iterations for jacob
        lambda2<-lambda #this is just reformating the data because we are doing a recursive loop
        lambda<-lambda1 #this is just reformating the data because we are doing a recursive loop
        print(c(i,mean(lambda2$Lambda-lambda1$Lambda))) #this prints the mean difference
      }
      else{
        lambda<-lambda1 #this is just reformating the data because we are doing a recursive loop
      }
    }
  }
  return(lambda1) #returns the output as the number of iterations determined by the user.
}

#### Practical experiments

iterative.bt.with.benchmark(2,2,id,lambda1,dataset,1000) #this benchmark function will print the average difference between the previous and new lambda and print this every 25 iterations until its under the threshold
lambda.update<-iterative.bt(2,2,id,lambda1,dataset,1000) #this doesn't print benchmark but does the math still

rank<-order(lambda$Lambda,lambda$DocId) #this rank orders our lambdas we used to generate the data
cbind(lambda,rank) #here we can see the lambdas with our ranks
rank2<-order(lambda.update$Lambda,lambda.update$DocId) #this rank orders the lambdas we got from all of our iterations
cbind(lambda.update,rank2) #here we can see the new ranks

#therefore, we can see that with a large enough dataset, we can derive the exact order of the lambdas we put in!

#the key here is just that you have a large enough dataset, and you have enough iterations. Thankfully one of those is taken care of by the code.
