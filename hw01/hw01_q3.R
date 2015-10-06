#############################
# < Yi Qu >
# STAT W4240 
# Homework <HW 01 , Problem <3>
# < Homework Due Date: Sep 16 >
#############################

#################
# Setup
#################
setwd("C:/Users/yi/Desktop/W4240/hw01")
library(MASS)

#################
# Problem a
#################
nrow(Boston)
ncol(Boston)
#each row represent each observations
#each column represent each feature or predictor

#################
# Problem b
#################
plot(Boston$dis,Boston$nox)
# nitrogen oxides concentration decrease as the weighted mean of distances to five Boston employment centres.
# It is reasonable, since the employment centres will produce nitrogen oxides.

#################
# Problem c
#################
cor(Boston,Boston$crim)
#Strong Correlation: rad, tax, 
#Weak Correlation: zn, indus, nox, rm, age, dis, ptratio, black, lstat, medv
#No Correlation: chas

#################
# Problem d
#################
sapply(Boston,range)
mean(Boston$crim)
mean(Boston$tax)
mean(Boston$ptratio)
hist(Boston$crim)
hist(Boston$tax)
hist(Boston$ptratio)
#some suburbs have particularly high crime rates,
#since the mean crim is 3.613524, but the range of crim up to 88.97620
#tax and ptratio do not habe particularly high crime
#since the mean is around the middle of range

#################
# Problem e
#################
sum(Boston$chas == 1)

#################
# Problem f
#################
median(Boston$ptratio)

#################
# Problem g
#################
which.min(Boston$medv) # find the row number of the lowest medv
min(Boston$medv) # print the value of the lowest medv
Boston[which.min(Boston$medv),] # print all the values in this row
medianBoston<-sapply(Boston[,num],median)
Boston[399,]-medianBoston   # compare the certain observation with the median of overall ranges


#################
# Problem h
#################
sum(Boston$rm>7)
sum(Boston$rm>8) # find the suburbs that rm > 8


