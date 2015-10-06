#############################
# < Yi Qu >
# STAT W4240 
# Homework 05 
# < Homework Due Date: Novemeber 25 >
#
# The following code use classfication trees and logistic regression
# to classify the federalist papers
#############################

#################
# Problem 4
#################
#----- START YOUR CODE BLOCK HERE -----#
gini = function(pm1){
return(pm1*(1-pm1)+(1-pm1)*pm1)
}

error = function(pm1){
n = length(pm1)
pm2 = 1 - pm1
pm = NULL
for(i in 1:n) {
if(pm1[i] - pm2[i] > 0)
pm[i] = pm2[i]
else
pm[i] = pm1[i]
}
return(pm)
}

entropy = function(pm1){
return(-pm1*log(pm1)-(1-pm1)*log(1-pm1))
}

pmk = seq(0,1,by = 0.01)
gini.pmk = gini(pmk)
error.pmk = error(pmk)
entropy.pmk = entropy(pmk)

plot(pmk,entropy.pmk,type="l",col='red',ylab="function(pmk)")
lines(pmk,error.pmk,type="l",col='blue')
lines(pmk,gini.pmk,type="l",col='green')
legend("bottom",c("cross-entropy","classification error","Gini index"), 
col=c("red","blue","green"),lty=1)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 6a
#################

#----- START YOUR CODE BLOCK HERE -----#
# load("C:\\Users\\yi\\Desktop\\W4240\\hw05\\.RData")
ls()
#dim(dtm.hamilton.train)
#nrow(dtm.hamilton.train)

dtm.train <- rbind(dtm.hamilton.train, dtm.madison.train)
dtm.train.label <- matrix(nrow = nrow(dtm.train),ncol = 1)
dtm.train.label[1:nrow(dtm.train),1] <- 0
dtm.train.label[1:nrow(dtm.hamilton.train),1] <- 1
dtm.train.df<- cbind(dtm.train.label,dtm.train)
words <- as.vector(mydictionary[,1])
colnames(dtm.train.df) <- c("y", words)
dtm.train.df = data.frame(dtm.train.df)
#fix(dtm.train.df)
#class(dtm.train.df[,1])

dtm.test <- rbind(dtm.hamilton.test, dtm.madison.test)
test.num <- nrow(dtm.test)
hamilton.num <- nrow(dtm.hamilton.test)
madison.num <- nrow(dtm.madison.test)
dtm.test.label <- matrix(nrow = test.num,ncol = 1)
dtm.test.label[1:test.num,1] <- 0
dtm.test.label[1:hamilton.num,1] <- 1
dtm.test.df<- cbind(dtm.test.label,dtm.test)
colnames(dtm.test.df) <- c("y", words)
dtm.test.df = data.frame(dtm.test.df)
#fix(dtm.test.df)
#class(dtm.test.df[,2])

#library(rpart)
fit.6a = rpart(y ~ ., data=dtm.train.df, parms = list(split='gini'))
out.6a = predict(fit.6a, dtm.test.df, type="class")
plot(fit.6a)
text(fit.6a, use.n = F)

correct = (sum(out.6a==0 & dtm.test.df$y==0)+ sum(out.6a==1 & dtm.test.df$y==1))/test.num
#0.962963 26/27
false.positive <- sum(out.6a==1 & dtm.test.df$y ==0)/madison.num #0.09 #1/11
false.negative <- sum(out.6a==0 & dtm.test.df$y ==1)/hamilton.num #0
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 6b
#################
#----- START YOUR CODE BLOCK HERE -----#
fit.6b = rpart(y ~ ., data=dtm.train.df, parms = list(split='information'))
out.6b = predict(fit.6a, dtm.test.df, type="class")
plot(fit.6b)
text(fit.6b, use.n = F)

correct = (sum(out.6b==0 & dtm.test.df$y==0)+ sum(out.6b==1 & dtm.test.df$y==1))/test.num
#0.962963 26/27
false.positive <- sum(out.6b==1 & dtm.test.df$y ==0)/madison.num #
false.negative <- sum(out.6b==0 & dtm.test.df$y ==1)/hamilton.num #
# do different between two models.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 7a
#################
#----- START YOUR CODE BLOCK HERE -----#
#dim(dtm.test.df)
n <- ncol(dtm.test.df)
dtm.train.scaled.df <- dtm.train.df
train.mean <- colMeans(dtm.train.df[,2:n])
train.sd <- apply(dtm.train.df[,2:n],2,sd)

dtm.train.scaled.df[,2:n] <- scale(dtm.train.df[,2:n])
dtm.train.scaled.df[is.na(dtm.train.scaled.df)] <- 0
fix(dtm.train.scaled.df)

save(dtm.train.scaled.df,file="train_scale.Rda")

#sum(is.na(dtm.train.scaled.df)==TRUE) # make sure NAN all gone
fix(dtm.test.df)
dtm.test.scaled.df <- dtm.test.df
dtm.test.scaled.df[,2:n] <- scale(dtm.test.df[,2:n],
center = train.mean, scale = train.sd)
dtm.test.scaled.df[is.na(dtm.test.scaled.df)] <- 0
fix(dtm.test.scaled.df)

save(dtm.test.scaled.df,file="test_scale.Rda")
# no only after centering and scaling, we can treat every X samely.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 7b
#################
#----- START YOUR CODE BLOCK HERE -----#
install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
# class(dtm.train.scaled.df)
dtm.train.scaled.mat <- data.matrix(dtm.train.scaled.df)

cv.fit.ridge <- cv.glmnet(dtm.train.scaled.mat[,2:n],dtm.train.scaled.mat[,1],
alpha=0, family="binomial")
best.lambda <- cv.fit.ridge$lambda.min # 3.14008



correct.ridge = (sum(pred.ridge==0 & dtm.test.df$y==0)+ sum(pred.ridge==1 & dtm.test.df$y==1))/test.num
false.positive.ridge <- sum(pred.ridge==1 & dtm.test.df$y ==0)/madison.num #
false.negative.ridge <- sum(pred.ridge==0 & dtm.test.df$y ==1)/hamilton.num #

correct.ridge
#[1] 0.5925926
false.positive.ridge 
#[1] 1
false.negative.ridge
#[1] 0

fit.ridge <- glmnet(dtm.train.scaled.mat[,2:n],dtm.train.scaled.mat[,1],
alpha=0,family="binomial")
index.best <- which(cv.fit.ridge$lambda == cv.fit.ridge$lambda.min)
sort.ridge.beta <-sort(abs(fit.ridge$beta[,index.best]),decreasing = TRUE)
sort.ridge.beta[1:10]
  februari       upon     whilst     within      sever      X1783       form 
0.01667850 0.01572522 0.01428038 0.01367439 0.01344405 0.01302053 0.01193143 
    member         X5   although 
0.01168189 0.01153821 0.01127392  
plot(fit.ridge,main="Ridge")
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 7c
#################
#----- START YOUR CODE BLOCK HERE -----#
cv.fit.lasso <- cv.glmnet(dtm.train.scaled.mat[,2:n],dtm.train.scaled.mat[,1],
alpha=1,family="binomial")
best.lambda <- cv.fit.lasso$lambda.min #  0.01155041
best.lambda
pred.lasso <- predict(cv.fit.lasso,newx=dtm.test.scaled.mat[,2:n],
s=best.lambda,type="class")

correct.lasso = (sum(pred.lasso==0 & dtm.test.df$y==0)+ sum(pred.lasso==1 & dtm.test.df$y==1))/test.num
false.positive.lasso <- sum(pred.lasso==1 & dtm.test.df$y ==0)/madison.num #
false.negative.lasso <- sum(pred.lasso==0 & dtm.test.df$y ==1)/hamilton.num #

correct.lasso
#[1] 0.8888889
false.positive.lasso 
#[1] 0.1818182
false.negative.lasso
#[1] 0.0625

fit.lasso <- glmnet(dtm.train.scaled.mat[,2:n],dtm.train.scaled.mat[,1],
alpha=1,family="binomial")
ind.best <- which(cv.fit.lasso$lambda == cv.fit.lasso$lambda.min) 
sort.lasso.beta <-sort(abs(fit.lasso$beta[,ind.best]),decreasing = TRUE)
sort.lasso.beta[1:10]
   whilst  februari      upon      form  although    within     sever    lesser 
1.7549559 1.6178042 1.3466915 0.7008493 0.6751041 0.5129032 0.4357153 0.4222712 
       ad      anim 
0.3212790 0.2833942 
plot(fit.lasso, main = "Lasso")

# The words are different, The coefficients of ridge is smaller than coefficient
# of lasso, because lasso will turn most of beta to 0.
#----- END YOUR CODE BLOCK HERE -----#
##########################################

#################
# End of Script
#################
