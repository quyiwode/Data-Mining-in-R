#############################
# < Yi Qu >
# STAT W4240 
# Homework 06 
# < Homework Due Date: Dec 8 >
# James 8.4.10
#############################

#################
# Problem 1a
#################
#----- START YOUR CODE BLOCK HERE -----#
setwd("C:/Users/yi/Desktop/W4240/hw06")
library(ISLR)
data("Hitters")
fix(Hitters)
dim(Hitters) # 322 20
class(Hitters) # data.frame
Hitters.new <-Hitters[complete.cases(Hitters$Salary),]
dim(Hitters.new) # 263 20
n <- nrow(Hitters.new)
fix(Hitters.new)
Hitters.new$Salary <- log(Hitters.new$Salary) # Hitters.new has been log.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1b
#################
#----- START YOUR CODE BLOCK HERE -----#
Hitters.train <- Hitters.new[1:200,]
Hitters.test <- Hitters.new[201:n,]
#fix(Hitters.train)
#fix(Hitters.test)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1c
#################
#----- START YOUR CODE BLOCK HERE -----#
library(gbm)
lambda <- c(0.0001,0.001,0.01,0.1,1)
train.mse <- NA
r <- length(lambda) 
for(i in 1: r) {
boost <- gbm(Salary~., data=Hitters.train, distribution="gaussian", 
n.trees=1000,shrinkage=lambda[i])
boost.hat <- predict(boost, newdata=Hitters.train,n.trees=1000)
train.mse[i] <- mean((boost.hat-Hitters.train$Salary)^2)
}
plot(log(lambda),train.mse,type="l") # decreasing line
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1d
#################
#----- START YOUR CODE BLOCK HERE -----#
test.mse <- NA
r <- length(lambda) 
for(i in 1: r) {
boost <- gbm(Salary~., data=Hitters.train, distribution="gaussian", 
n.trees=1000,shrinkage=lambda[i])
boost.hat <- predict(boost, newdata=Hitters.test,n.trees=1000)
test.mse[i] <- mean((boost.hat-Hitters.test$Salary)^2)
}
plot(log(lambda),test.mse,type="l") # decreasing line
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1e
#################
#----- START YOUR CODE BLOCK HERE -----#
# The best test MSE of boosting is when lamba = 0.1
mse.boosting <- test.mse[4] # 0.2580242
# Chapter 3  # choosing Simple Linear Regression
lmH = lm(Salary~., data = Hitters.train)
lm.hat <- predict(lmH, newdata = Hitters.test)
lm.mse <- mean((lm.hat-Hitters.test$Salary)^2) # 0.4917959
# Chapter 6 # choose Lasso
library(glmnet)
# transform data.frame in to matrix
Hitters.train.mat <- data.matrix(Hitters.train)
Hitters.test.mat <- data.matrix(Hitters.test)
#fix(Hitters.test.mat)
cv.fit.lasso = cv.glmnet(Hitters.train.mat[,1:19],Hitters.train.mat[,20],
alpha=1)
best.lambda <- cv.fit.lasso$lambda.min # 0.02155417 
pred.lasso <- predict(cv.fit.lasso,newx=Hitters.test.mat[,1:19],s=best.lambda)
lasso.mse <- mean((pred.lasso-Hitters.test.mat[,20])^2) # 0.04746479
#----- END YOUR CODE BLOCK HERE -----#
# Lasso MSE = 0.04746479
# Simple Linear Regression MSE = 0.4917959
# Boosting MSE = 0.2580242

#################
# Problem 1f
#################
#----- START YOUR CODE BLOCK HERE -----#
boost.best <- gbm(Salary~., data=Hitters.train, distribution="gaussian", 
n.trees=1000,shrinkage=lambda[4])
summary(boost.best)
# Best three variables:
                var    rel.inf
CAtBat       CAtBat 20.2609949
CRuns         CRuns 11.0214634
PutOuts     PutOuts  8.7152460
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1g
#################
#----- START YOUR CODE BLOCK HERE -----#
library(randomForest)
bag = randomForest(Salary~., data=Hitters.train, mtry=19,importance= TRUE)
bag.hat <- predict(bag, newdata = Hitters.test)
bag.mse <- mean((bag.hat-Hitters.test$Salary)^2) # 0.2297212 
# The MSE of Bagging is little better than Boosting.
#----- END YOUR CODE BLOCK HERE -----#
##########################################

#################
# End of Script
#################
