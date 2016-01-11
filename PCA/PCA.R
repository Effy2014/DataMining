setwd("/Users/XW/Desktop/datamining-hw")
library(pixmap)
############getting matrix##########################################################
# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = 1:38
view_list = c(  'P00A+000E+00', 'P00A+005E+10' , 'P00A+005E-10' , 'P00A+010E+00')

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths
len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)

# Find the total number of pixels in a picture

# Pre-allocate a matrix with dimensions (number of pictures) x (number of pixels per picture)
faces_matrix = vector()
# Load all of the pictures; you can use code from homework 1
for (i in pic_list){
    for (j in 1:4) {
        filename = sprintf("CroppedYale/%s/%s_%s.pgm",
                           dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
        face = read.pnm(filename)
        face_vector = getChannels(face)
        face_vectors = as.vector(face_vector)
        faces_matrix = rbind(faces_matrix, face_vectors)
    }
}
dim(faces_matrix) #152 32256

# Convert the matrix into a vector via as.vector and back with dim()
# Example:
# A = rbind(c(1,2,3),c(5,3,1))
# print(A)
# original.dimensions.A = dim(A)
# a = as.vector(A)
# print(a)
# dim(a) = original.dimensions.A
# print(a)


# Use colMeans() on your matrix to get "mean face" vector
# Convert back to original size using dim()

mean_face <- colMeans(faces_matrix)
mean_face_vector = mean_face
dim(mean_face_vector) <- dim(face_vector)
mean_face <- pixmapGrey(mean_face_vector)
# now plot the data
plot(mean_face)
# give it a nice title
title('mean_face')
# save the result
filename = 'mean_face.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#PCA
library(pixmap)
# Run prcomp() on your centered face matrix
centered = mat.or.vec(dim(faces_matrix)[1], dim(faces_matrix)[2])
for (i in 1:dim(faces_matrix)[1]){
    centered[i,] <- faces_matrix[i,] - as.vector(mean_face_vector)
}
#pic_pca = apply(faces_matrix, 1, function(x){x-as.vector(mean_face_vector)})

principal <- prcomp(centered)
plot(principal)
n_comp = length(principal$sdev)
comp_var = mat.or.vec(n_comp, 1)
for (i in 1:n_comp){
    if (i == 1){
        comp_var[i] = principal$sdev[i]^2
    }
    else{
        comp_var[i] = principal$sdev[i]^2 + comp_var[i-1]
    }
}
comp_var = comp_var/comp_var[n_comp] *100
plot(comp_var, xlim = c(0, n_comp), 
     xlab = "number of components", ylab = "proportion of the variance explained")
abline(h = 100 , col = "red")
title("result of PCA")
filename = 'result of PCA.png'
dev.copy(device = png, file = filename, height = 600, width = 800)
dev.off()

# display first 9 eigenfaces in a 3-by-3 grid 
eigen_matrix = c()
for (i in 1:3){
    row = c()
    for (j in 1:3){
        temp = principal$rotation[, (i-1)*3 + j]
        dim(temp) = dim(face_vector)
        row = cbind(row, temp)
    }
    eigen_matrix = rbind(eigen_matrix, row)
}
eigenfaces = pixmapGrey(eigen_matrix)
plot(eigenfaces)
# give it a nice title
title('eigenfaces')
# save the result
filename = 'eigenfaces using first nine components.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
#Use the eigenfaces to reconstruct yaleB01 P00A+010E+00.pgm. 
#Starting with the mean face, add in one eigenface (weighted principal component) at a time 
#until reach 24 eigenfaces

# Find the index of face yaleB01_P00A+010E+00.pgm
face_index = 4
# Often, reading in the names and storing them as a list is a good idea
# Use the scores and loadings you found in 2c to reconstruct a face 
# by adding in 1 bases at a time
face_vector_add = mean_face
face_row = mean_face_vector
face_ma = c()
for (m in 1:24){
    face_re = mat.or.vec(1, dim(principal$rotation)[1])
    for (j in 1:dim(principal$rotation)[1]){
        face_re[j] = principal$x[face_index,m]*principal$rotation[j,m]
    }
   face_vector_add = face_vector_add + face_re
   face_ma_add = face_vector_add
   dim(face_ma_add) = dim(mean_face_vector)
   face_row = cbind(face_row, face_ma_add)
   if(m%%5==4){
       face_ma = rbind(face_ma,face_row)
       face_row = c()
   }
} 
represted = pixmapGrey(face_ma)
plot(represted)
# give it a nice title
title('reconstructed faces adding one eigenface')
# save the result
filename = 'reconstructing face yaleB01_P00A+010E+00.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# by adding in 5 bases at a time

face_vector_add = mean_face
face_row = mean_face_vector
face_ma = c()
for (m in seq(1,120,1)){
    face_re = mat.or.vec(1, dim(principal$rotation)[1])
    for (j in 1:dim(principal$rotation)[1]){
        face_re[j] = principal$x[face_index,m]*principal$rotation[j,m]
    }
    face_vector_add = face_vector_add + face_re
    if(m%%5 == 0){
        face_ma_add = face_vector_add
        dim(face_ma_add) = dim(mean_face_vector)
        face_row = cbind(face_row, face_ma_add)
    }
    if(m%%25 == 20){
        face_ma = rbind(face_ma,face_row)
        face_row = c()
    }
} 
represted = pixmapGrey(face_ma)
plot(represted)
title('reconstructed faces adding five eigenface')
filename = 'reconstructing face yaleB01_P00A+010E+00(adding five eigenface).png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

####################################################################
# Find the index of the faces to remove
# Find the index of face yaleB05_P00A+010E+00.pgm
# Remove pictures from matrix; run prcomp()
new_mat <- faces_matrix[setdiff(1:nrow(faces_matrix), c(17:20)),]
new_mat_shape <- dim(new_mat)
new_mat_colmean <- colMeans(new_mat)
new_mat_centered <- mat.or.vec(new_mat_shape[1], new_mat_shape[2])
for (i in 1:new_mat_shape[1]){
    new_mat_centered[i,] = new_mat[i,] - new_mat_colmean  
}
principal_new <- prcomp(new_mat_centered)
#getting yaleB05 P00A+010E+00.pgm score
original <- faces_matrix[20,]
original_centered <- original-new_mat_colmean
score = mat.or.vec(1,dim(principal_new$rotation)[2])
for (i in 1:dim(principal_new$rotation)[2]){
    score[i] = principal_new$rotation[,i]%*%original_centered
}
#reconstruct yaleB05 P00A+010E+00.pgm score by score*loading
reconstructed <- new_mat_colmean
for (i in 1:dim(new_mat)[1]){
    reconstructed = reconstructed + score[i]*principal_new$rotation[,i]
}

dim(reconstructed) = dim(mean_face_vector)
dim(original) = dim(mean_face_vector)
compare <- cbind(original, reconstructed)
face_new = pixmapGrey(compare)
plot(face_new)
title('comparing original face and reconstructed face')
filename = 'comparing original face and reconstructed face.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
