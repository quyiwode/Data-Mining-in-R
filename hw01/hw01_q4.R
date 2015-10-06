#############################
# < Yi Qu >
# STAT W4240 
# Homework <HW 01> , Problem <4>
# < Homework Due Dat : Sep 16 >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("C:/Users/yi/Desktop/W4240/CroppedYale")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
install.packages("pixmap")
library(pixmap)

#################
# Problem 4a
#################

# paste or type in the given code here
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")

# now plot the data
plot(face_01)
# give it a nice title
title('hw01_01a: the first face')
# save the result
filename = 'hw01_01a.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# extract the class and size

#----- START YOUR CODE BLOCK HERE -----#
class(face_01) # the class is pixmapGrey
face_01 #the size is 192*168
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4b
#################

# make face_01 into a matrix with the given command
face_01_matrix = getChannels(face_01)

# load a second face
face_02 = read.pnm(file = "CroppedYale/yaleB02/yaleB02_P00A-005E+10.pgm")
face_02_matrix = getChannels(face_02)

# combine two faces into a single data matrix and make that a pixmap
faces_matrix = cbind( face_01_matrix , face_02_matrix )
faces = pixmapGrey( faces_matrix )

# plot to verify
plot(faces)

# find min and max values 

#----- START YOUR CODE BLOCK HERE -----#
max(faces_matrix) 
# for this class, the max = 1 color = white
min(faces_matrix)
# for this class the min = 0 color = black

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4c
#################

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths

#----- START YOUR CODE BLOCK HERE -----#
dir_list_1 # the directory under CroppedYale/
dir_list_2 # all the directory under CoppedYale/, with recursion
length(dir_list_1) # the num of dir_list_1 is 38
length(dir_list_2) # the num of dir_list_2 is 2547
dir_list_1[1:2] # example elements of dir_list_1
dir_list_2[1:2] # example elements of dir_list_1
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = c( 05 , 11 , 31 )
view_list = c(  'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')

# preallocate an empty list
pic_data = vector("list",length(pic_list)*length(view_list))
# initialize an empty matrix of faces data
faces_matrix = vector()

#----- START YOUR CODE BLOCK HERE -----#
i=1
j=1
for(i in 1:length(pic_list))
{
for(j in 1:length(view_list))
{
filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
face = read.pnm(file=filename)
pic_data[[3*(i-1)+j]] = getChannels(face)
}
}
faces_matrix_row1=cbind(pic_data[[1]],pic_data[[2]],pic_data[[3]])
faces_matrix_row2=cbind(pic_data[[4]],pic_data[[5]],pic_data[[6]])
faces_matrix_row3=cbind(pic_data[[7]],pic_data[[8]],pic_data[[9]])

faces_matrix=rbind(faces_matrix_row1,faces_matrix_row2,faces_matrix_row3)

#----- END YOUR CODE BLOCK HERE -----#

# now faces_matrix has been built properly.  plot and save it.
faces = pixmapGrey(faces_matrix)
plot(faces)
# give it a nice title
title('hw01_01d: 3x3 grid of faces')
# save the result
filename = 'hw01_01d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# End of Script
#################


