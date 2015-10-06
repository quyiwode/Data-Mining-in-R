#############################
# < Yi Qu >
# STAT W4240 
# Homework 06 
# < Homework Due Date: Dec 8 >
# Question 3
#############################

#################
# Problem 3b
#################
#----- START YOUR CODE BLOCK HERE -----#
setwd("C:/Users/yi/Desktop/W4240/hw06")
save(dtm.train.df,file="train_df.Rda")
save(dtm.test.df,file="test_df.Rda")
load("train_df.Rda")
load("test_df.Rda")
fix(dtm.test.df)
fix(dtm.train.df)
dtm.train.mat <- data.matrix(dtm.train.df)
dtm.test.mat <- data.matrix(dtm.test.df)
n.hamilton.train = sum(dtm.train.df[,1]==1) # 35
n.madison.train = sum(dtm.train.df[,1]==0) # 15
# p(y=y)
prob.hamilton <- 35/50
prob.madison <- 15/50
# p(x=k)
words <- colSums(dtm.train.mat[,2:4876])
totalwords <- sum(words) #53957
words.prob <- words/totalwords
# p(x=k|y=y)
fix(dtm.train.mat)
hamilton.words <- colSums(dtm.train.mat[1:35,2:4876])
madison.words <- colSums(dtm.train.mat[36:50,2:4876])
length(hamilton.words)
hamilton.totalwords <- sum(hamilton.words) # 36421
madison.totalwords <- sum(madison.words) # 17536
hamilton.words.prob <- hamilton.words/hamilton.totalwords
madison.words.prob <- madison.words/madison.totalwords
# caculate mutual information
mutual.information <-  
hamilton.words.prob*prob.hamilton*log(hamilton.words.prob/words.prob)+
(1-hamilton.words.prob)*prob.hamilton*
log((1-hamilton.words.prob)/(1-words.prob))+
madison.words.prob*prob.madison*log(madison.words.prob/words.prob)+
(1-madison.words.prob)*prob.madison*
log((1-madison.words.prob)/(1-words.prob))
# turn into dataframe and get the order
temp <- data.frame(mutual.information)
words.order <- order(temp$mutual.information, decreasing = TRUE)

# set all NA into 0
fix(dtm.train.df)
dtm.train.df.noY <-dtm.train.df[,-1]
fix(dtm.train.df.noY)
dtm.train.df.noY.order <- dtm.train.df.noY[,words.order]
fix(dtm.train.df.noY.order)
dtm.train.df.order <- cbind(dtm.train.df$y,dtm.train.df.noY.order)
names(dtm.train.df.order)[1]<-"y"
fix(dtm.train.df.order)

fix(dtm.test.df)
dtm.test.df.noY <-dtm.test.df[,-1]
fix(dtm.test.df.noY)
dtm.test.df.noY.order <- dtm.test.df.noY[,words.order]
fix(dtm.test.df.noY.order)
dtm.test.df.order <- cbind(dtm.test.df$y,dtm.test.df.noY.order)
names(dtm.test.df.order)[1]<-"y"
fix(dtm.test.df.order)

# Tree with Gini
Tree.Gini.result <- matrix(0,4,3)
colnames(Tree.Gini.result) <- c("Correct","False Positive","False Negative")
rownames(Tree.Gini.result) <- c("200","500","1000","2500")

library(rpart)
subset.num <- c(200,500,1000,2500)
# i = 1
for(i in 1:4){
n <- subset.num[i]+1
subset.train = dtm.train.df.order[,1:n]
subset.test = dtm.test.df.order[,1:n]
fit.gini <- rpart(y~.,data=subset.train,parms = list(split='gini'))
out.gini <- predict(fit.gini,subset.test,type="class")

correct = (sum(out.gini==0 & dtm.test.df.order$y==0)+ 
sum(out.gini==1 & dtm.test.df.order$y==1))/27
false.positive <- sum(out.gini==1 & dtm.test.df.order$y ==0)/11
false.negative <- sum(out.gini==0 & dtm.test.df.order$y ==1)/16

Tree.Gini.result[i,1] <- correct
Tree.Gini.result[i,2] <- false.positive
Tree.Gini.result[i,3] <- false.negative
}

      Correct False Positive False Negative
200  0.962963     0.09090909              0
500  0.962963     0.09090909              0
1000 0.962963     0.09090909              0
2500 0.962963     0.09090909              0

# Tree with Information
Tree.Info.result <- Tree.Gini.result
for(i in 1:4){
n <- subset.num[i]+1
subset.train = dtm.train.df.order[,1:n]
subset.test = dtm.test.df.order[,1:n]
fit.info <- rpart(y~.,data=subset.train,parms = list(split='information'))
out.info <- predict(fit.info,subset.test,type="class")

correct = (sum(out.info==0 & dtm.test.df.order$y==0)+ 
sum(out.info==1 & dtm.test.df.order$y==1))/27
false.positive <- sum(out.info==1 & dtm.test.df.order$y ==0)/11
false.negative <- sum(out.info==0 & dtm.test.df.order$y ==1)/16

Tree.Info.result[i,1] <- correct
Tree.Info.result[i,2] <- false.positive
Tree.Info.result[i,3] <- false.negative
}
      Correct False Positive False Negative
200  0.962963     0.09090909              0
500  0.962963     0.09090909              0
1000 0.962963     0.09090909              0
2500 0.962963     0.09090909              0

# center and scale the data
load("train_scale.Rda")
load("test_scale.Rda")
dtm.train.scaled.noY <- dtm.train.scaled.df[,-1]
dtm.train.scaled.order.noY <- dtm.train.scaled.noY[,words.order]
dtm.train.scaled.order <- cbind(dtm.train.scaled.df$y,dtm.train.scaled.order.noY)
names(dtm.train.scaled.order)[1]<-"y"
fix(dtm.train.scaled.order)

dtm.test.scaled.noY <- dtm.test.scaled.df[,-1]
dtm.test.scaled.order.noY <- dtm.test.scaled.noY[,words.order]
dtm.test.scaled.order <- cbind(dtm.test.scaled.df$y,dtm.test.scaled.order.noY)
names(dtm.test.scaled.order)[1]<-"y"
fix(dtm.test.scaled.order)

# Ridge Classification
Ridge.result <- Tree.Gini.result
library(glmnet)
class(dtm.train.scaled.order)
dtm.train.scaled.order.mat <- data.matrix(dtm.train.scaled.order)
dtm.test.scaled.order.mat <- data.matrix(dtm.test.scaled.order)

for(i in 1:4){
n <- subset.num[i]+1
subset.train = dtm.train.scaled.order.mat[,1:n]
subset.test = dtm.test.scaled.order.mat[,1:n]

cv.fit.ridge <- cv.glmnet(subset.train[,2:n],subset.train[,1], 
alpha=0, family="binomial")
best.lambda <- cv.fit.ridge$lambda.min
pred.ridge <- predict(cv.fit.ridge,newx=subset.test[,2:n],
s=best.lambda,type="class")

correct.ridge = (sum(pred.ridge==0 & dtm.test.scaled.order$y==0)+
sum(pred.ridge==1 & dtm.test.scaled.order$y==1))/27
false.positive.ridge <- sum(pred.ridge==1 & dtm.test.scaled.order$y ==0)/11 #
false.negative.ridge <- sum(pred.ridge==0 & dtm.test.scaled.order$y ==1)/16 #

Ridge.result[i,1] <- correct.ridge
Ridge.result[i,2] <- false.positive.ridge
Ridge.result[i,3] <- false.negative.ridge
}

       Correct False Positive False Negative
200  0.5925926              1              0
500  0.5925926              1              0
1000 0.5925926              1              0
2500 0.5925926              1              0

# Lasso Classification
Lasso.result <- Tree.Gini.result

for(i in 1:4){
n <- subset.num[i]+1
subset.train = dtm.train.scaled.order.mat[,1:n]
subset.test = dtm.test.scaled.order.mat[,1:n]

cv.fit.lasso <- cv.glmnet(subset.train[,2:n],subset.train[,1], 
alpha=1, family="binomial")
best.lambda <- cv.fit.lasso$lambda.min
pred.lasso <- predict(cv.fit.lasso,newx=subset.test[,2:n],
s=best.lambda,type="class")

correct.lasso = (sum(pred.lasso==0 & dtm.test.scaled.order$y==0)+
sum(pred.lasso==1 & dtm.test.scaled.order$y==1))/27
false.positive.lasso <- sum(pred.lasso==1 & dtm.test.scaled.order$y ==0)/11 #
false.negative.lasso <- sum(pred.lasso==0 & dtm.test.scaled.order$y ==1)/16 #

Lasso.result[i,1] <- correct.lasso
Lasso.result[i,2] <- false.positive.lasso
Lasso.result[i,3] <- false.negative.lasso
}
       Correct False Positive False Negative
200  0.8518519      0.3636364              0
500  0.8148148      0.4545455              0
1000 0.8148148      0.4545455              0
2500 0.8888889      0.2727273              0
#----- END YOUR CODE BLOCK HERE -----#
# Correct
plot(subset.num,Lasso.result[,1],type="l",col="1",main="Correct",
ylab="Rate",ylim = c(0.4,1))
lines(subset.num,Ridge.result[,1],type="l",col="2")
lines(subset.num,Tree.Gini.result[,1],type="l",col="3",lwd=2)
lines(subset.num,Tree.Info.result[,1],type="l",lwd=2,lty=3)
legend("bottom",c("Lasso","Ridge","Tree Gini","Tree Info"),
col=c(1,2,3,1),lty=c(1,1,1,3))

# False negative
plot(subset.num,Lasso.result[,2],type="l",col="1",main="False Positive",
ylab="Rate",ylim = c(0,1))
lines(subset.num,Ridge.result[,2],type="l",col="2")
lines(subset.num,Tree.Gini.result[,2],type="l",col="3",lwd=2)
lines(subset.num,Tree.Info.result[,2],type="l",lwd=2,lty=3)
legend("right",c("Lasso","Ridge","Tree Gini","Tree Info"),
col=c(1,2,3,1),lty=c(1,1,1,3))

# False positive
plot(subset.num,Lasso.result[,3],type="l",col="1",main="False Negative",
ylab="Rate",ylim = c(0,1))
lines(subset.num,Ridge.result[,3],type="l",col="2")
lines(subset.num,Tree.Gini.result[,3],type="l",col="3",lwd=2)
lines(subset.num,Tree.Info.result[,3],type="l",lwd=2,lty=3)
legend("right",c("Lasso","Ridge","Tree Gini","Tree Info"),
col=c(1,2,3,1),lty=c(1,1,1,3))
####################################
#############################
#################
# End of Script
#################