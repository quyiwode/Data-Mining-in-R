#############################
# < Yi Qu >
# STAT W4240 
# Homework <HW 03> , Problem <3>
# < Homework Due Dat : Oct 14 >
#############################

#################
# Setup
#################
setwd("C:/Users/yi/Desktop/W4240/hw03")

#################
# Problem 3a
#################
#----- START YOUR CODE BLOCK HERE -----#
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3b
#################
#----- START YOUR CODE BLOCK HERE -----#
eps <- rnorm(100, mean = 0, sd = 0.5)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3c
#################
#----- START YOUR CODE BLOCK HERE -----#
y <- -1 + 0.5*x + eps
length(y) #the length of y is 100
# beta0 = -1, beta1 = 0.5
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3d
#################
#----- START YOUR CODE BLOCK HERE -----#
plot(x,y)
# x and y are linear associated.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3e
#################
#----- START YOUR CODE BLOCK HERE -----#
fit <- lm(y~x)
coef(fit)
#(Intercept)           x 
# -1.0188463   0.4994698 
# smaller beta0 and smaler beta1.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3f
#################
#----- START YOUR CODE BLOCK HERE -----#
plot(x,y)
abline(fit,col = "red",lwd = 3)
abline(a=-1,b= 0.5,col = "blue", lwd = 3)
legend(0,-2.0, # places a legend at the appropriate place 
c("least squares line","population regression line"), # puts text in the legend 
lty=c(1,1), # gives the legend appropriate symbols (lines)
lwd=c(3,3),col=c("red","blue")) # gives the legend lines the correct color and width
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3g
#################
#----- START YOUR CODE BLOCK HERE -----#
fit2 <- lm(y~x+I(x^2))
anova(fit,fit2)

# The null hypothesis is two models fit the data equally,
# since p-value = 0.1638, so the two models seem equally, 
# the quadratic term do not contribute much.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3h
#################
#----- START YOUR CODE BLOCK HERE -----#
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
eps.less <- rnorm(100, mean = 0, sd = 0.1)
y.less <- -1 + 0.5*x + eps.less
fit.less <- lm(y.less~x)
plot(x,y.less)
abline(fit.less,col = "red",lwd = 3)
abline(a=-1,b= 0.5,col = "blue", lwd = 3)
legend(-2,0, c("least squares line","population regression line"), lty=c(1,1),lwd=c(3,3),col=c("red","blue")) 
fit2.less <- lm(y.less~x+I(x^2))
anova(fit.less,fit2.less)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3i
#################
#----- START YOUR CODE BLOCK HERE -----#
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
eps.more<- rnorm(100, mean = 0, sd = 0.8)
y.more <- -1 + 0.5*x + eps.more
fit.more <- lm(y.more~x)
plot(x,y.more)
abline(fit.more,col = "red",lwd = 3)
abline(a=-1,b= 0.5,col = "blue", lwd = 3)
legend(0,-2, c("least squares line","population regression line"), lty=c(1,1),lwd=c(3,3),col=c("red","blue")) 
fit2.more <- lm(y.more~x+I(x^2))
anova(fit.more,fit2.more)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3j
#################
#----- START YOUR CODE BLOCK HERE -----#
confint(fit)
confint(fit.less)
confint(fit.more)
# the noisier data has wider confidence intervals, vice versa.
# the confidence interval are related to SSE.
#----- END YOUR CODE BLOCK HERE -----#





