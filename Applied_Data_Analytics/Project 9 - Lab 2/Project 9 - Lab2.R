"Lab 2 - Project 9"

rm(list=ls()) # clears the memory

#A,
"tumor_imaging.dat is not formatted, we don't know the column names, also one can
identify that many rows are missing, and there are mission values.
whereas brain size data contains column names and it is formatted."

X <- scan("https://raw.githubusercontent.com/adamsky777/R_projects/main/Data/tumor_imaging.dat")
"Scan is a primitive command scans all the infromation into a variable X.
X therefore a single long vector containing all the numbers given in tumor_imaging.dat"



numbers = (1:16)
testMatrix_By_Row =  matrix(data = numbers, nrow = 4, ncol = 4, byrow=TRUE)
# creates a 4x4 matrix for the numbers, and inserts them by rows

testMatrix_NOT_By_Row =  matrix(data = numbers, nrow = 4, ncol = 4, byrow=FALSE)
# creates a 4x4 matrix for the numbers, and inserts them by columns
testMatrix_By_Row2 =  matrix(data = numbers, nrow = 2, ncol = 8, byrow=TRUE)
# creates a 2x8 matrix for the numbers, and inserts them by rows

testMatrix_NOT_By_Row2 =  matrix(data = numbers, nrow = 2, ncol = 8, byrow=FALSE)
# creates a 2x8 matrix for the numbers, and inserts them by columns


testMatrix_By_Row
testMatrix_By_Row[1,1]
testMatrix_By_Row[4,4]
testMatrix_By_Row[4, ] #access to the 4th row
testMatrix_By_Row[ ,4] # access tot he 3th col

testMatrix_By_Row[ ,-4] # access to all columns 'cept the 4th
testMatrix_By_Row[ ,-2] # access to all columns 'cept the 2nd
testMatrix_By_Row[, c(4,2,3)] # access to colum 4, 2, 3
testMatrix_By_Row[,(1:3)] # access to column from 1 to 3


#D,
"Then write R-code which outputs the second, third and fourth column of 
testMatrixbyrow. 
Then write code which outputs the first and second row of testMatrixbyrow."



testMatrix_By_Row[, c(2,3,4)] # prints the 2nd, 3rd and 4th colum.
testMatrix_By_Row[c(1,2), ] # prints the 1st and 2nd row


#E describe what the following code does.

T <- 265
n = 39

Xmat <- matrix(data=X, nrow= 3*T, ncol= n, byrow = TRUE)

" creates a matrix into a variable Xmat, with the data of X and specifies the
number of rows to 3*T where T is variable, and specifies the number of colums with
the ncol, by assigning a variable n, furthermore the command inserts the data into 
the matrix by rows."

Xmat[1,1]
head(Xmat)
Xmat[2,]

# F what values found for mouse number 39?

Xmat[, 39]
unique(Xmat[, 39])
# all measurements for mouse 39 = 0

#G, Remove mouse 39 from the data set.
Xmat <- Xmat[, -39]
dim(Xmat)

#H, 
# max uptakes
max.index = (1:T)*3-2


#I,
# mean and median uptakes
mean.index = max.index + 1
meadian.index = max.index + 2



#J

tumor.max <- Xmat[max.index, ]
tumor.mean <- Xmat[mean.index, ]
tumor.median <- Xmat[meadian.index ,]
dim(tumor.max)
dim(tumor.mean)
dim(tumor.median)

# Transpose the variables
# t() is the Matrix transpose command

tumor.max <- t(tumor.max)
dim(tumor.max)
tumor.mean <- t(tumor.mean)
tumor.median <- t(tumor.median)
dim(tumor.median)

# K compute the average over the 38 mice of the maximum uptakes at time t = 1.

mean(tumor.max[,1])

# L compute the avg uptakes over the 38 mice at the time period t=2, t=3. t=4, and t=5

mean(tumor.max[,2])
mean(tumor.max[,3])
mean(tumor.max[,4])
mean(tumor.max[,5])

# M , make a variable meansOfMaxes filled with NA's that is T = 265 long.
meansOfMaxes <- NULL
meansOfMaxes <- rep(NA, T) 
"rep command replicates elements of vectors or list,  T times" 

for (i in (1:T)){
  meansOfMaxes[i] <- mean(tumor.max[, i])
}

# N, meansOfMeans, meansOfMedians

meansOfMeans <- rep(NA, T)
for (i in {1:T}){
  meansOfMeans[i] <- mean(tumor.mean[, i])   
  
}
meansOfMedians <- rep(NA, T)
for (i in (1: T)){
  meansOfMedians[i] <- mean(tumor.median[, i])
}


par(mfrow=c(1,1))
plot(meansOfMaxes, type = "l", col=1, lty=1 , ylab="means", ylim=c(0,max(meansOfMaxes)))
points(meansOfMeans, type = "l", col=2, lty=2, ylab = "Means")
points(meansOfMedians, type = "l", col="blue", lty=3, ylab = "Means")
legend("bottomright", c("Means of Maxes", "Means of means", "Means of Medians"),
       col = c(1,2,3), lty = c(1,2,3))
# NB: Points vs plot
"points add points to a plot, whereas plot command plots a new plot"


" an easier way to use colMeans command() which retruns the means for each columns"
# EXAMPLE
head(USArrests)
colMeans(USArrests)

#here in the dataset we have 
ncol(tumor.max)
" 265 columns , with the command colMeanbs we calculate all the means for all the 
265 columns and we plot them as shown below."

plot(colMeans(tumor.max), type="l", ylim=c(0,max(meansOfMaxes)), ylab="Means")
points(colMeans(tumor.mean), type="l", col=2,lty=2)
points(colMeans(tumor.median), type="l", col="blue",lty=3)
legend("bottomright", c("Means of maxes", "Means of means", "Means of medians"), col=c(1,2,3), lty=c(1,2,3))
