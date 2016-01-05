setwd("/Users/XW/Desktop/datamining")
# install the package into your R distribution.  From the
#install.packages("pixmap")
library(pixmap)
# paste or type in the given code here
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
# now plot the data
plot(face_01)
# give it a nice title
title('face_01')
# save the result
filename = 'yaleB01.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
# extract the class and size
attr(face_01, 'class')
attr(face_01, 'size')
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
max(faces_matrix)
#1
min(faces_matrix)
#0.007843137
#1 is white 0 is black 

#load in all of data by looping through the folders and sorting the values in a list
# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE) #read all the folders in CroppedYale
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE) #read all files included in CroppedYale
# find lengths
length(dir_list_1)
#38
length(dir_list_2)
#2547

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = c( 05 , 11 , 31 )
view_list = c(  'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')

# preallocate an empty list
pic_data = vector("list",length(pic_list)*length(view_list))
# initialize an empty matrix of faces data
faces_matrix = vector()


for (i in 1:3){
    face_vectors = c()
    for (j in 1:3) {
        filename = sprintf("CroppedYale/%s/%s_%s.pgm",
                           dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
        face = read.pnm(filename)
        face_vector = getChannels(face)
        face_vectors = cbind(face_vectors, face_vector)
    }
    faces_matrix = rbind(faces_matrix, face_vectors)
}

# now faces_matrix has been built properly.  plot and save it.
faces = pixmapGrey(faces_matrix)
plot(faces)
# give it a nice title
title('3x3 grid of faces')
# save the result
filename = '3x3 grid of faces.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()


