install.packages("rstan")
library(rstan)
setwd("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModel")
load("fitExperiment2.7")
#We need to compare the output of our model to pst.lambda, the output of their stan model
post.lambda = summary(fitExperiment2.7)$summary[paste0('a[',1:50,']'),'mean']
