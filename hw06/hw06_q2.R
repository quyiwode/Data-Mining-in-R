#################
# Problem 2a
#################
#----- START YOUR CODE BLOCK HERE -----#
X1 <- c(3,2,4,1,2,4,4)
X2 <- c(4,2,4,4,1,3,1)
X <- cbind(X1,X2)
Y <- c("Red","Red","Red","Red","Blue","Blue","Blue")
plot(X,col=Y)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2b
#################
#----- START YOUR CODE BLOCK HERE -----#
abline(a = -0.5, b =1)
#y = x - 0.5
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2c
#################
#----- START YOUR CODE BLOCK HERE -----#
b0 = -0.5, b1 = 1, b2 = -1 
When -0.5 + X1 - X2 >0, classify to Red, and when -0.5 + X1 - X2 <0,
 classify to Blue.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2d
#################
#----- START YOUR CODE BLOCK HERE -----#
abline(a=0,b=1,lty=2,col="red")
abline(a=-1,b=1,lty=2,col="blue")
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2e
#################
#----- START YOUR CODE BLOCK HERE -----#
The support vectors for the maximal margin classifer is 
(2,1),(2,2),(4,3),(4,4).
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2f
#################
#----- START YOUR CODE BLOCK HERE -----#
Since the seventh observation is (4,1), which is far away from the hyperplane,
so a slight movement of it will not affect the maximal margin hyperplane.
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2g
#################
#----- START YOUR CODE BLOCK HERE -----#
plot(X,col=Y)
abline(a=0.5,b=0.7)
The formula of the not optimal hyperplance could be:
X2=0.7X1+0.5
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 2h
#################
#----- START YOUR CODE BLOCK HERE -----#
plot(X,col=Y)
points(x=2,y=3,pch = 15)
The dark square observation will make the two classes no longer separable by a
hyperplane.
#----- END YOUR CODE BLOCK HERE -----#