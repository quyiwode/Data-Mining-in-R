#############################
# < Yi Qu >
# STAT W4240 
# Homework 04 
# < Homework Due Date: Novemeber 11 >
#
# The following code analyzes the federalist papers
#############################

#################
# Problem 1a
#################

#----- START YOUR CODE BLOCK HERE -----#
# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("C:/Users/yi/Desktop/W4240/hw04/hw04")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(tm)

source('hw04.R')

preprocess.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_hamilton_train')
preprocess.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_hamilton_test')
preprocess.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_madison_train')
preprocess.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_madison_test')
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1b
#################
#----- START YOUR CODE BLOCK HERE -----#
hamilton.train <- read.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_hamilton_train_clean')
hamilton.test <- read.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_hamilton_test_clean')
madison.train <- read.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_madison_train_clean')
madison.test <- read.directory('C:/Users/yi/Desktop/W4240/hw04/hw04/fp_madison_test_clean')
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1c
#################
#----- START YOUR CODE BLOCK HERE -----#
myfull <- c(hamilton.train, hamilton.test, madison.train, madison.test)
mydictionary <- make.sorted.dictionary.df(myfull)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1d
#################
#----- START YOUR CODE BLOCK HERE -----#
dtm.hamilton.train <- make.document.term.matrix(hamilton.train,mydictionary)
dtm.hamilton.test <- make.document.term.matrix(hamilton.test,mydictionary)
dtm.madison.train <- make.document.term.matrix(madison.train,mydictionary)
dtm.madison.test <- make.document.term.matrix(madison.test,mydictionary)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1e
#################
#----- START YOUR CODE BLOCK HERE -----#
mu <- 100/nrow(mydictionary)
logp.hamilton.train <- make.log.pvec(dtm.hamilton.train,mu)
logp.hamilton.test <- make.log.pvec(dtm.hamilton.test,mu)
logp.madison.train <- make.log.pvec(dtm.madison.train,mu)
logp.madison.test <- make.log.pvec(dtm.madison.test,mu)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2
#################
#----- START YOUR CODE BLOCK HERE -----#
# computer naive bayes and return response, 1 for hamilton, 0 for madison.
naive.bayes = function(logp.hamilton.train, logp.madison.train,
log.prior.hamilton, log.prior.madison, dtm.test)
{
bayes.response <- mat.or.vec(nrow(dtm.test),1)
bayes.hamilton <- log.prior.hamilton + dtm.test %*% logp.hamilton.train 
bayes.madison <- log.prior.madison + dtm.test %*% logp.madison.train
bayes.compare <- bayes.hamilton - bayes.madison
  for(i in 1: nrow(dtm.test))
  {
    if(bayes.compare[i] > 0)
    {
    bayes.response[i] <- 1
    }
  }
return(bayes.response)
}
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3
#################
#----- START YOUR CODE BLOCK HERE -----#
num.hamilton.train <- nrow(dtm.hamilton.train)
num.madison.train <-nrow(dtm.madison.train)
num.hamilton.test <- nrow(dtm.hamilton.test)
num.madison.test <-nrow(dtm.madison.test)

log.prior.hamilton <- log(num.hamilton.train/(num.hamilton.train+num.madison.train))
log.prior.madison <- log(num.madison.train/(num.hamilton.train+num.madison.train))

response.hamilton.test <- naive.bayes(logp.hamilton.train, logp.madison.train,
log.prior.hamilton, log.prior.madison, dtm.hamilton.test)
response.madison.test <- naive.bayes(logp.hamilton.train, logp.madison.train,
log.prior.hamilton, log.prior.madison, dtm.madison.test)

true.positive <- sum(response.hamilton.test==1)/num.hamilton.test #1
true.negative <- sum(response.madison.test==0)/num.madison.test # 0.64
false.positive <- sum(response.madison.test==1)/num.madison.test # 0.36
false.negative <- sum(response.hamilton.test==0)/num.hamilton.test #0
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4a
#################
#----- START YOUR CODE BLOCK HERE -----#
# set mu
mu <- 1/nrow(mydictionary)
# initialize matrix to store rate for correct, FP, FN
correct.matrix <- mat.or.vec(5,5)
false.positive.matrix <- mat.or.vec(5,5)
false.negative.matrix <- mat.or.vec(5,5)
# i for five different mu
for(i in 1:5)
{
# random row of data
dtm.hamilton.random <- dtm.hamilton.train[sample(nrow(dtm.hamilton.train)),]
dtm.madison.random <- dtm.madison.train[sample(nrow(dtm.madison.train)),]
num.hamilton.testrow <- num.hamilton.train*0.2
num.madison.testrow <- num.madison.train*0.2
# j for 5-fold CV
   for(j in 1:5)
   {
   # get train and test subset of data
   dtm.hamilton.random.test <- dtm.hamilton.random[(1+(j-1)*num.hamilton.testrow):(j*num.hamilton.testrow),]
   dtm.hamilton.random.train <- dtm.hamilton.random[-(1+(j-1)*num.hamilton.testrow):-(j*num.hamilton.testrow),]

   dtm.madison.random.test <- dtm.madison.random[(1+(j-1)*num.madison.testrow):(j*num.madison.testrow),]
   dtm.madison.random.train <- dtm.madison.random[-(1+(j-1)*num.madison.testrow):-(j*num.madison.testrow),]
  
   # get log probability from train subset
   logp.hamilton.CV.train <- make.log.pvec(dtm.hamilton.random.train,10^(i-1)*mu)
   logp.madison.CV.train <- make.log.pvec(dtm.madison.random.train,10^(i-1)*mu)
  
   # computer naive bayes, and return response, 1 for hamilton, 0 for madison. 
   response.hamilton.CV <- naive.bayes(logp.hamilton.CV.train, logp.madison.CV.train,
   log.prior.hamilton, log.prior.madison, dtm.hamilton.random.test)
   response.madison.CV <- naive.bayes(logp.hamilton.CV.train, logp.madison.CV.train,
   log.prior.hamilton, log.prior.madison, dtm.madison.random.test)
   
   # compute three rates 
   correct <- (sum(response.hamilton.CV==1)+sum(response.madison.CV==0))/(num.hamilton.testrow+num.madison.testrow)
   false.positive <- sum(response.madison.CV==1)/num.madison.testrow
   false.negative <- sum(response.hamilton.CV==0)/num.hamilton.testrow
   correct.matrix[j,i] <- correct
   false.positive.matrix[j,i] <- false.positive
   false.negative.matrix[j,i] <- false.negative
   }
}

par(mfrow=c(3,2))
colmean<- apply(correct.matrix, 2, mean) 
barplot(colmean,main = "Barplot of mean correct classification rate",
names.arg=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
boxplot(correct.matrix, main = "Boxplot of correct classification rate",
names=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))

#par(mfrow=c(1,2))
colmean<- apply(false.positive.matrix, 2, mean) 
barplot(colmean,main = "Barplot of mean false positive rate",
names.arg=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
boxplot(false.positive.matrix, main = "Boxplot of false positive rate",
names=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))

#par(mfrow=c(1,2))
colmean<- apply(false.negative.matrix, 2, mean) 
barplot(colmean,main = "Barplot of mean false negative rate",
names.arg=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
boxplot(false.negative.matrix, main = "Boxplot of false negative rate",
names=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
#----- END YOUR CODE BLOCK HERE -----#


#################
# Problem 4c
#################
#----- START YOUR CODE BLOCK HERE -----#
correct.all.matrix <- mat.or.vec(1,5)
false.positive.all.matrix <- mat.or.vec(1,5)
false.negative.all.matrix <- mat.or.vec(1,5)
for(i in 1:5)
{
logp.hamilton.all.train <- make.log.pvec(dtm.hamilton.train,10^(i-1)*mu)
logp.madison.all.train <- make.log.pvec(dtm.madison.train,10^(i-1)*mu)
response.hamilton.all <- naive.bayes(logp.hamilton.all.train, logp.madison.all.train,
log.prior.hamilton, log.prior.madison, dtm.hamilton.test)
response.madison.all <- naive.bayes(logp.hamilton.all.train, logp.madison.all.train,
log.prior.hamilton, log.prior.madison, dtm.madison.test)
correct <- (sum(response.hamilton.all==1)+sum(response.madison.all==0))/(num.hamilton.test+num.madison.test)
false.positive <- sum(response.madison.all==1)/num.madison.test
false.negative <- sum(response.hamilton.all==0)/num.hamilton.test
correct.all.matrix[1,i] <- correct
false.positive.all.matrix[1,i] <- false.positive
false.negative.all.matrix[1,i] <- false.negative
}
#plot bar plot of three conditions.
par(mfrow=c(3,1))
barplot(correct.all.matrix, main = "Barplot of correct classification rate",
names=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
barplot(false.positive.all.matrix, main = "Barplot of false positive rate",
names=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
barplot(false.negative.all.matrix, main = "Barplot of false negative rate",
names=c('1','10','100','1000','10000'),xlab = "mu/dictionary", 
ylab ="Rate",ylim = range(0:1))
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################
#----- START YOUR CODE BLOCK HERE -----#
# percentage error = abs(predict-true)/true
correct.CV <- apply(correct.matrix, 2, mean) 
correct.percentage.error <- abs(correct.all.matrix-correct.CV)/correct.all.matrix
fp.CV <- apply(false.positive.matrix, 2, mean) 
fp.percentage.error <- abs(false.positive.all.matrix-fp.CV)/false.positive.all.matrix
fn.CV <- apply(false.negative.matrix, 2, mean) 
fn.percentage.error <- abs(false.negative.all.matrix-fn.CV)/false.negative.all.matrix

#> fn.percentage.error
#     [,1] [,2] [,3] [,4] [,5]
#[1,]  NaN  NaN  NaN    1  NaN
#> fp.percentage.error
#     [,1]       [,2] [,3]     [,4]      [,5]
#[1,] 0.12 0.08333333  0.1 2.666667 0.4666667
#> correct.CV
#[1] 0.76 0.80 0.88 0.90 0.72

#----- END YOUR CODE BLOCK HERE -----#
##########################################

#################
# End of Script
#################
