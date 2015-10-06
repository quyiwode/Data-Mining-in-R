#############################
# < Yi Qu >
# STAT W4240 
# Homework <HW 02> , Problem <1>
# < Homework Due Dat : Sep 30 >
#############################

#################
# Setup
#################
setwd("C:/Users/yi/Desktop/W4240/hw02")

#################
# Problem 1a
#################
#----- START YOUR CODE BLOCK HERE -----#
tb1<-read.csv("hw02_q1_p1_fall14.csv",header=T)
fix(tb1)
dim(tb1)
rowmeans<-apply(tb1,1,mean)
columnmeans<-apply(tb1,2,mean)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1b
#################
#----- START YOUR CODE BLOCK HERE -----#
center.tb1<-scale(tb1,scale=FALSE)
center.tb1[1:5,]
#equal to tb1-columnmeans
#center.tb1 <- apply(tb1,2,function(y)y-mean(y))
cov.matrix<-cov(center.tb1)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1c
#################
#----- START YOUR CODE BLOCK HERE -----#
eigen(cov.matrix)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1d
#################
#----- START YOUR CODE BLOCK HERE -----#
pc = princomp(tb1,scores=T)
names(pc)
pc$loadings[1:5,]
pc$scores[1:5,]
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1e
#################
#----- START YOUR CODE BLOCK HERE -----#
names(pc)
x<-c('Comp1','Comp2','Comp3', 'Comp4', 'Comp5')
plot(factor(x),pc$sdev^2/sum(pc$sdev^2),
ylab="Proportion of Variance")
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1f
#################
#----- START YOUR CODE BLOCK HERE -----#
tb2<-read.csv("hw02_q1_p2_fall14.csv",header=T)
pc2 = princomp(tb2,scores=T)
tb2.scores <- pc2$scores
tb2.scores
tb2.loadings <-pc2$loadings
fix(tb2.loadings)

tb2.columnmeans<-apply(tb2,2,mean)
center.tb2 <- scale(tb2,scale=FALSE)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1g
#################
#----- START YOUR CODE BLOCK HERE -----#
#what is the first two scores, columns? yes!
two.center.tb2 <- tb2.scores[,1:2] %*% t(pc2$loadings[,1:2])
two.center.tb2[1:5,]
center.tb2
error <- center.tb2-two.center.tb2
for(i in 1:5)
Euclidean[i]<-sqrt(sum((error[i,])^2))
Euclidean
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1h
#################
#----- START YOUR CODE BLOCK HERE -----#
# what is direction for error??
error %*% t(two.center.tb2)
# orthogonality
#----- END YOUR CODE BLOCK HERE -----#





