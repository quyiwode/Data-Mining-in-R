#############################
# < Yi Qu >
# STAT W4240 
# Homework <HW 01 , Problem <2>
# < Homework Due Date: Sep 16 >
#############################

#################
# Setup
#################
setwd("C:/Users/yi/Desktop/W4240/hw01")
Auto=read.table(file.choose(),header=T,na.string="?")

Auto=na.omit(Auto)
dim(Auto)

#################
# Problem a
#################
sapply(Auto,class)

#################
# Problem b
#################
nums<-sapply(Auto,is.numeric)
sapply(Auto[,nums],range)

#################
# Problem c
#################
sapply(Auto[,nums],mean)
sapply(Auto[,nums],sd)

#################
# Problem d
#################
Auto2=Auto[-10,]
Auto3=Auto2[-85,]
sapply(Auto3[,nums],mean)
sapply(Auto3[,nums],sd)

#################
# Problem e
#################
#the scatterplot leads to the overview of any two variables of whole dataframe
pairs(Auto)
#the plot leads to more clear relationship between certain two variables
plot(Auto$weight,Auto$horsepower)
#the hist leads to distrubution of one variable
hist(Auto$horsepower)
#comment on my findings

#################
# Problem f
#################
cor(Auto[1:8],Auto[1])
#Yes from the correlation, we find as mpg have strong negative correlation with cyclinders, displacement,horsepowers and weights.
#and have strong positive correlation with year and origin, weak positive correlation with acceleration.
#which all will contribute to the prediction of mpg.





