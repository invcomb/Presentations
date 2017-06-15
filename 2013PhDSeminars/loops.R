#Loops


#Simple "for" loop
l <- matrix(0, 10, 1)  #Simple loop that does the same as "matrix (1:10)" 	
l
for (i in 1:10) {	     #in MATLAB written as "for i = 1:10", in SAS "do i=1 to 10"????
	l[i] <- i

}
l


x <- matrix (rnorm(200), 20)	#A matrix with 20 rows and 10 columns of radndom N(0,1)
c <- rep (0:1, c(5, 5))	#A vector with 5 zeros and 5 ones
A <- rbind (c, x)		#Combine the matrix and the vector
A				#Print matrix A


max <- max(A)		#Function that gives the maximum value in matrix A
max


m <- length (A [,1]) 	#Number of rows
n <- length (A [1,]) 	#Number of columns



#Loop with "for"
MC <-matrix (0, n, 1) 	#A column vector with n zeros
MC
for (i in 1:n) {		#A loop to give the maximum value for each column in A	
	MC[i] <- max (A[1:m, i])
}
MC				#Print matrix "MC" maximum value at each column 


MR <-matrix (0, m, 1) 	#A column vector with m zeros
MR
for (i in 1:m) {		#A loop to give the maximum value for each row in A	
	MR[i] <- max (A[i, 1:n])
}
MR				 #Print matrix "MR" maximum value at each row



#Loop with "apply"
MaxC <- apply(A, 2, max) #Gives the maximum value for each column in A as a vector
MaxC				 #Print vector "maxc" maximum value at each column  

MaxR <- apply(A, 1, max) #Gives the maximum value for each row in A as a vector
MaxR				 #Print vector "maxr" maximum value ot each row 	 					

mya <- array (1:24, dim = c(2,3,4))	#Gives an array with two rows, three columns, and four "dimension"
mya
max1 <- apply(mya, 1, max)		#Gives the maximum value for each row 
max1 
max2 <- apply(mya, 2, max)		#Gives the maximum values for each column
max2 
max3 <- apply(mya, 3, max)		#Gives the maximum value for each "dimension"
max3 



#Loop with "sapply"
fibonacci <- function(n){	#Function to generate Fibonacci numbers (example from The R Book)
		a <- 1
		b <- 0
		while (n > 0){
			swap <- a
			a <- a + b
			b <- swap
			n <- n - 1
		}
		b
}
v <- c(1:15)			#Vector with values from 1 to 15
sapply (v, fibonacci) 		#Gives a vector with Fibonacci numbers (could also give a vector directly "sapply (1:10, fibonacci)"



#Loop with "lapply"
c <- seq(1, 16, 3)		#vector seq(start value, end value, step)
d <- seq(15, 0, -3)
e <- seq(-15, 5, 5)
list <- list(c, d, e)		#combine the vectors to a list
list 			
maxlist <- lapply (list, max)	#Gives a list with the maximum value for each vector in the list	
maxlist	

sml <- sapply (list, max)	#Gives a vector with the maximum values for each vector in the list
sml



#Loop with "if" and "else"
minmax <- matrix (0, n, 1)
minmax 			#Print minmax
for (i in 1:n) {		#Loop to give maximum value for the column if the first row is equal to zero otherwise give the minimum value
	if (A[1, i] == 0)
		{minmax[i] <- max (A[2:m, i])}	
	else
		{minmax[i] <- min (A[2:m, i])}
}
minmax			#Print matrix "minmax"



#Loop with "while" and "indicater variable"
f <- 1
g <- 5
while (g > 1) {		#Loop that continues as long as g is larger than 1
	f <- f * g
	g <- g - 1		#Indicator variable that decreases with each iteration
}
f



#Loop with "while" and "break"
z = c(1, 2, 3, 4, 5, 6, -1, 7, 8, 9)
p <- 0
q <- 0
while (p == 0){		#Loop that continues as long as f == 0, forever...
	q = q + 1
		if (z[q] == -1){
 		break		#Unless a value in z == -1, than it stops
		}
	else{
	}
} 
q



#Loop with "repeat" and "break"
r <- 1
s <- 5
repeat {			#Loop that repeat itself until s is smaller than 2
	if (s < 2) break
		r <- r*s
		s <- s - 1		
}
r



#Taking the time to compare "for" with "apply"
M <- matrix (rnorm(200000), 2000)	#A matrix with 2000 rows of random N(0,1)

mm <- length (M [,1]) 			#Number of rows
mm
nn <- length (M [1,]) 			#Number of columns
nn

MaxRow <-matrix (0, mm, 1) 		#A column vector with mm zeros
pc <- proc.time()				#Command for taking the time
for (i in 1:mm) {				#A loop to give the maximum value for each row in A	
	MaxRow[i] <- max (M[i, 1:nn])
}
proc.time()-pc
 
system.time (apply (M, 1, max))	#Command for taking the time


#A more time consuming example
TRow <-matrix (0, mm, 1) 		#A column vector with mm zeros
pc <- proc.time()				#Command for taking the time
for (i in 1:mm) {				#A loop to give the t statistic for each row in A	
	TRow[i] <- t.test (M[i, 1:nn])$statistic
}
proc.time()-pc

system.time (apply (M, 1, t.test))



#The ifelse function
p <- rnorm(100)
p
pt <- ifelse (p < 0, -1, 1)		#Recode p into either negative or positive value 
pt

#ifelse also works for matrixes 
u <- matrix (rnorm(100), 10)
u
ut <- ifelse (u < 0, -1, 1)		#Recode u into either negative or positive value 
ut

#
system.time (MinMax <- ifelse (A[1,] == 0, max (A[2:m,]), min (A[2:m,])))
MinMax

#Loop with "if" and "else"
minmax <- matrix (0, n, 1)
minmax 			#Print minmax
pc <- proc.time()
for (i in 1:n) {		#Loop to give maximum value for the column if the first row is equal to zero otherwise give the minimum value
	if (A[1, i] == 0)
		{minmax[i] <- max (A[2:m, i])}	
	else
		{minmax[i] <- min (A[2:m, i])}
}
proc.time()-pc
minmax			#Print matrix "minmax"








