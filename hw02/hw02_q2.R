#############################
# < Yi Qu >
# STAT W4240 
# Homework <HW 02> , Problem <2>
# < Homework Due Date: Sep 30 >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# NOTE: It is acceptable to use code from previous solutions;
# just give it credit or load previous file via source()

# make sure R is in the proper working directory
# note that this will be a different path for every machine
#setwd("~/Documents/academic/teaching/STAT_W4240_2014_SPRG/hw/hw01")
setwd("C:/Users/yi/Desktop/W4240/CroppedYale")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pixmap)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = 1:38
view_list = c( 'P00A+000E+00', 'P00A+005E+10' , 'P00A+005E-10' , 'P00A+010E+00')

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths
len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)

#################
# # Problem 2a
#################
# Find the total number of pixels in a picture
face <- read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
face_matrix = getChannels(face)
num.pixels<-length(face_matrix)

# Pre-allocate a matrix with dimensions (number of pictures) x (number of pixels per picture)
pic_matrix <- matrix(, nrow=length(pic_list)*length(view_list), ncol=num.pixels)
# Load all of the pictures; you can use code from homework 1
i=1
j=1
for(i in 1:length(pic_list))
{
for(j in 1:length(view_list))
{
filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
face = read.pnm(file=filename)
face.matrix <- getChannels(face)
original.dimensions.face = dim(face.matrix)
face.vector = as.vector(face.matrix)
pic_matrix[4*(i-1)+j,] = face.vector
}
}

# Convert the matrix into a vector via as.vector and back with dim()
# Example:
# A = rbind(c(1,2,3),c(5,3,1))
# print(A)
# original.dimensions.A = dim(A)
# a = as.vector(A)
# print(a)
# dim(a) = original.dimensions.A
# print(a)

#################
# # Problem 2b
#################
# Use colMeans() on your matrix to get "mean face" vector
# Convert back to original size using dim()
meanface_matrix <- colMeans(pic_matrix)
length(meanface_matrix)

# the function to find the centered matrix
center_apply <- function(x) {
    apply(x, 2, function(y) y - mean(y))
}
pic_mean <- center_apply(pic_matrix)
dim(meanface_matrix)=original.dimensions.face
meanface = pixmapGrey( meanface_matrix )
plot(meanface)
title('hw02_02b: meanfaces')
# save the result
filename = 'hw02_02b.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # Problem 2c
#################
# Run prcomp() on your centered face matrix
pic_pca <- prcomp(pic_mean)
summary(pic_pca)
#screeplot(pic_pca)

plot(pic_pca$sdev^2/sum(pic_pca$sdev^2),
xlab="Comp",ylab="Proportion of Variance",pch=3,)
title('hw02_02c: pic_pca')
# save the result
filename = 'hw02_02c.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # Problem 2d
#################
# Build your eigenface grid like the face grid in homework 1
#names(pic_pca)
pic_pca_scores<-pic_pca$x
pic_pca_loadings<-pic_pca$rotation
dim(pic_pca_scores)
dim(pic_pca_loadings)
#test <- pic_pca_scores %*% t(pic_pca_loadings)
#test[1:5,1:5]
#pic_mean[1:5,1:5]

pic_data = vector("list",length(pic_list)*length(view_list))
faces_matrix = vector()
i=1
j=1
for(i in 1:3){
this_row = vector()
for(j in 1:3)
{
n=3*(i-1)+j
eigenface <- pic_pca_loadings[,n]
dim(eigenface)=original.dimensions.face
pic_data[[3*(i-1)+j]] = eigenface
this_face = pic_data[[3*(i-1)+j]]
this_row = cbind(this_row, this_face)
}
faces_matrix = rbind( faces_matrix , this_row )
}

faces = pixmapGrey(faces_matrix)
plot(faces)
filename = 'hw02_02d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # Problem 2e
#################
# Find the index of face yaleB01_P00A+010E+00.pgm
# Often, reading in the names and storing them as a list is a good idea
# Use the scores and loadings you found in 2c to reconstruct a face 
# by adding in 1 (or 5) bases at a time

#find the right face, and prove it is right! oneface and testface are same, right!
#testface.center.matrix <- pic_pca_scores[4,1:30] %*% t(pic_pca_loadings[,1:30])
#dim(testface.center.matrix)= original.dimensions.face
#testface.matrix = testface.center.matrix + meanface_matrix
#testface = pixmapGrey(testface.matrix)
#plot(testface)
#testface.true <- read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
#plot(testface.true)

# add one eigenface each time
oneface.matrix = matrix(nrow=192,ncol=168)
one_faces_matrix = vector()
i=1
j=1
this_face = meanface_matrix
for(i in 1:5){
this_row = vector()
for(j in 1:5)
{
n1=5*(i-1)+j
this_row = cbind(this_row, this_face)
oneface.center.matrix <- pic_pca_scores[4,1:n1] %*% t(pic_pca_loadings[,1:n1])
dim(oneface.center.matrix)= original.dimensions.face
this_face = meanface_matrix + oneface.center.matrix
}
one_faces_matrix = rbind(one_faces_matrix , this_row)
}
one_faces <- pixmapGrey(one_faces_matrix)
plot(one_faces)

# add five eigenface each time
fiveface.matrix=matrix(nrow=192,ncol=168)
five_faces_matrix = vector()
i=1
j=1
this_face = meanface_matrix
for(i in 1:5){
this_row = vector()
for(j in 1:5)
{
n2<-25*(i-1)+5*j
fiveface.center.matrix <- pic_pca_scores[4,1:n2] %*% t(pic_pca_loadings[,1:n2])
dim(fiveface.center.matrix)= original.dimensions.face
this_row = cbind(this_row, this_face)
this_face = meanface_matrix + fiveface.center.matrix
}
five_faces_matrix = rbind(five_faces_matrix , this_row )
}
five_faces = pixmapGrey(five_faces_matrix)
plot(five_faces)

#################
# # Problem 2f
#################
# Find the index of the faces to remove
# Find the index of face yaleB05_P00A+010E+00.pgm
# Remove pictures from matrix; run prcomp()
dir_list_new <- dir_list_1[-5]
pic_list_new = 1:37
pic_matrix_new <- matrix(, nrow=length(pic_list_new)*length(view_list), ncol=num.pixels)
i=1
j=1
for(i in 1:length(pic_list_new))
{
for(j in 1:length(view_list))
{
filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_new[pic_list_new[i]] , dir_list_new[pic_list_new[i]] , view_list[j])
face = read.pnm(file=filename)
face.matrix <- getChannels(face)
#original.dimensions.face = dim(face.matrix)
face.vector = as.vector(face.matrix)
pic_matrix_new[4*(i-1)+j,] = face.vector
}
}
fix(pic_matrix_new)

# center my new data
meanface_new <- colMeans(pic_matrix_new)
dim(meanface_new)=original.dimensions.face
pic_new_mean <- center_apply(pic_matrix_new)
pca_new <- prcomp(pic_new_mean)

face5 <- read.pnm(file = "CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm")
plot(face5)
face5.matrix <- getChannels(face5)

face5.vector <- as.vector(face5.matrix - meanface_new)
face5.onematrix <-matrix(face5.vector,nrow=1)
face5.onematrix[1:5]
# get the score of target image
score5 <- face5.onematrix %*% pca_new$rotation
#reconstructe the image use new loadings:
rface5.matrix <- score5 %*% t(pca_new$rotation)
rface5.matrix[1:5]
dim(rface5.matrix)= original.dimensions.face
rface5 <- pixmapGrey(rface5.matrix + meanface_new)
plot(rface5)

#################
# # End of Script
#################