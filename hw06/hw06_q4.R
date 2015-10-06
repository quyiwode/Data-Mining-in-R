#############################
# < Yi Qu >
# STAT W4240 
# Homework 06 
# < Homework Due Date: Dec 8 >
#  Question 4
#############################

#################
# Problem 4a
#################
#----- START YOUR CODE BLOCK HERE -----#
setwd("C:/Users/yi/Desktop/W4240/hw06")
save(dtm.hamilton.train,file="hamilton_train.Rda")
save(dtm.madison.train,file="madison_train.Rda")
save(dtm.hamilton.test,file="hamilton_test.Rda")
save(dtm.madison.test,file="madison_test.Rda")
save(dtm.train.scaled.df,file="train.Rda")
save(dtm.test.scaled.df,file="test.Rda")

load("train.Rda")
load("test.Rda")
load("madison_test.Rda")
load("hamilton_test.Rda")

dtm.test.scaled.df[,1] <- factor(dtm.test.scaled.df[,1])
dtm.train.scaled.df[,1] <- factor(dtm.train.scaled.df[,1])
# class(dtm.test.scaled.df[,1])
# class(dtm.train.scaled.df[,1])
fix(dtm.train.scaled.df)
library(e1071)
dtm.train.4a <- dtm.train.scaled.df[,1:101]
dtm.test.4a <- dtm.test.scaled.df[,1:101]
tune.out = tune(svm,y~.,data=dtm.train.4a,kernel="linear",
ranges=list(cost=c(0.01,0.1,0.5,1,5,10,100,200)))
summary(tune.out) # cost=0.1 or larger, error rate is same.
svmfit <- svm(y~., data=dtm.train.4a, kernel="linear", cost=0.1)
# svmfit$index
y.hat <- predict(svmfit, dtm.test.4a)
test.num = nrow(dtm.test.4a)
madison.num = nrow(dtm.madison.test)
hamilton.num = nrow(dtm.hamilton.test)
correct.svm = (sum(y.hat==0 & dtm.test.4a$y==0)+
 sum(y.hat==1 & dtm.test.4a$y==1))/test.num 
false.positive.svm <- sum(y.hat==1 & dtm.test.4a$y ==0)/madison.num 
false.negative.svm <- sum(y.hat==0 & dtm.test.4a$y ==1)/hamilton.num 
correct.svm
# 0.8518519
false.positive.svm
# 0.3636364
false.negative.svm
# 0
# similar with lasso regression. Maybe the variables are too much???
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4b
#################
#----- START YOUR CODE BLOCK HERE -----#
svm.word = function(words.num,train,test)
{
col.num <- words.num + 1
dtm.train.4b <- train[,1:col.num]
dtm.test.4b <- test[,1:col.num]
tune.out.4b = tune(svm,y~.,data=dtm.train.4b,kernel="linear",
ranges=list(cost=c(0.01,0.1,0.5,1,5,10,100)))
best.4b = tune.out.4b$best.model
ypred = predict(best.4b, dtm.test.4b)
correct.4b = (sum(ypred==0 & dtm.test.4b$y==0)+
sum(ypred==1 & dtm.test.4b$y==1))/test.num 
false.positive.4b <- sum(ypred==1 & dtm.test.4b$y ==0)/madison.num 
false.negative.4b <- sum(ypred==0 & dtm.test.4b$y ==1)/hamilton.num 
performance <- c(correct.4b,false.positive.4b,false.negative.4b)
return(performance)
}

# svm.word(100,dtm.train.scaled.df,dtm.test.scaled.df)

words = seq(5,100,by=5)
correct.4b = NA
false.positive.4b = NA
false.negative.4b = NA
for (i in 1: length(words))
{
performance <- svm.word(words[i],dtm.train.scaled.df,dtm.test.scaled.df)
correct.4b[i] <- performance[1]
false.positive.4b[i] <- performance[2]
false.negative.4b[i] <- performance[3]
}

par(mfrow=c(3,1))
plot(words,correct.4b,type="l",col='red')
plot(words,false.positive.4b,type="l",col='blue')
plot(words,false.negative.4b,type="l",col='green')
# Reach the best when words.num = 40-60 highest corrct rate = 0.9629630
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4c
#################
#----- START YOUR CODE BLOCK HERE -----#

svm.word.RBF = function(words.num,train,test)
{
col.num <- words.num + 1
dtm.train.4b <- train[,1:col.num]
dtm.test.4b <- test[,1:col.num]
tune.out.4b = tune(svm,y~.,data=dtm.train.4b,
ranges=list(cost=c(0.01,0.1,0.5,1,5,10,100)))
best.4b = tune.out.4b$best.model
ypred = predict(best.4b, dtm.test.4b)
correct.4b = (sum(ypred==0 & dtm.test.4b$y==0)+
sum(ypred==1 & dtm.test.4b$y==1))/test.num 
false.positive.4b <- sum(ypred==1 & dtm.test.4b$y ==0)/madison.num 
false.negative.4b <- sum(ypred==0 & dtm.test.4b$y ==1)/hamilton.num 
performance <- c(correct.4b,false.positive.4b,false.negative.4b)
return(performance)
}

correct.4c = NA
false.positive.4c = NA
false.negative.4c = NA
for (i in 1: length(words))
{
performance <- svm.word.RBF(words[i],dtm.train.scaled.df,dtm.test.scaled.df)
correct.4c[i] <- performance[1]
false.positive.4c[i] <- performance[2]
false.negative.4c[i] <- performance[3]
}

par(mfrow=c(3,1))
plot(words,correct.4c,type="l",col='red')
plot(words,false.positive.4c,type="l",col='blue')
plot(words,false.negative.4c,type="l",col='green')
# best correct rate = 0.9259259 when words.num=45-40
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################
#----- START YOUR CODE BLOCK HERE -----#
dtm.train.4d <- dtm.train.scaled.df[,c("y","upon","depart")]
fix(dtm.train.4d)
plot(dtm.train.4d[,3],dtm.train.4d[,2],col=dtm.train.4d$y)
svm.4d <- svm(y~., data=dtm.train.4d, type ="C", kernel ="linear")
plot(svm.4d,dtm.train.4d,xlim = c(-0.6,6.5),ylim=c(-1.3,3))

# Interpret the resulting figure?? why it does not show all the dots.
#----- END YOUR CODE BLOCK HERE -----#
##########################################
library(MASS)
data(cats,package="MASS")
plot(cats[,2:3],col=cats$Sex)
m <- svm(Sex~., data= cats)
#plot(m,cats)
lines(m$fitted)
#################
# End of Script
#################