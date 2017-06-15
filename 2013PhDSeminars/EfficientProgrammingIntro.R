###############################################################################
###
### INTRODUCING EFFICIENT R PROGRAMMING
###
### Feng Li <feng.li@stat.su.se>
###
### Wed Feb 27 15:37:40 CET 2013
###
###############################################################################

###----------------------------------------------------------------------------
### General programming tips
###----------------------------------------------------------------------------

## Pre-allocate the storage if possible

## Choose  data structure, matrix, data.frame, list

## Vectorization of code (avoid loops)
m <- 1e3
n <- 1e3
A <- matrix(rnorm(m*n), m, n)
B <- matrix(rnorm(m*n), m, n)
C <- matrix(NA, m, n)

tic <- proc.time()
for(i in 1:m)
  {
    for(j in 1:n)
    {
      C[i, j] <- (A[i, j])^2 +(B[i, j])^2
    }
  }
proc.time()-tic

tic <- proc.time()
## system.time(C <- A^2+B^2)
C <- A^2+B^2
proc.time() -tic

###----------------------------------------------------------------------------
### EFFICIENT LOOPS
###----------------------------------------------------------------------------

## apply() # apply a function to the margin of matrix
A0 <- apply(A, 2, sd) # standard deviation for each column of A

myfun <- function(x) {length(x[x>0])}
A1 <- apply(A, 2, myfun) # length of positive numbers of each column

## lapply() # apply to a function to each element of a list
myLst <- list(A = A, B = B, C = C)
LstMean <- lapply(myLst, mean) # mean of each element in the list
LstMin <- lapply(myLst, min)
LstMax <- lapply(myLst, max)
LstMyFun <- lapply(myLst, colMeans)

## mapply() # apply a function to more than one list
myLstFun <- function(x, y) {x-y}
mapply(FUN = myLstFun, x = LstMax, y = LstMin)

## rapply() # recursive apply to a list

BigLst <- list(LstMean, LstMax, LstMin)
BigLstFun <- function(x) {round(abs(x))}
rapply(BigLst, BigLstFun, how = "replace")
rapply(BigLst, BigLstFun, how = "unlist")

###----------------------------------------------------------------------------
### DEBUGGING
###----------------------------------------------------------------------------

## A check if a special year contains a leap day (Feb 29)
## 2004 (yes) 2005(no) 1900(no) 2000(yes)
isLeapday <- function(year)
  {
   mod4 <- year%%4
   mod100 <- year%%100
   mod400 <- year%%400

   # if((mod4  == 0 & mod100 != 0) |  mod400  == 0)
   if(mod4  == 0 & mod100 != 0)
     {
       out <- TRUE
     }else
     {
       out <- FALSE
     }
   return(out)
  }
isLeapday(2004)
isLeapday(2005)
isLeapday(1900)
isLeapday(2000)


## browser()
## Use keyboard n: next, c: continue, Q: quit the browser
isLeapday <- function(year)
  {
   mod4 <- year%%4
   mod100 <- year%%100
   mod400 <- year%%400
   # browser()
   if((mod4  == 0 & mod100 != 0) |  mod400  == 0)
   # if(mod4  == 0 & mod100 != 0)
     {
       out <- TRUE
     }else
     {
       out <- FALSE
     }
   return(out)
  }
isLeapday(2000)

## traceback()
fun1 <- function(x) { print(1); fun2(2) }
fun2 <- function(x) { x + a.variable.which.does.not.exist }
fun1(2) # gives a strange error
traceback() # trace back to functions until error happens

## try()
TestFun <- function(x) {x^2}
TestFun("Feng")

TestFun <- function(x)
  {
    out <- try(x^2, silent = TRUE) # wrap the suspicious code

    if(is(out, "try-error"))
      {
        ## Check if there was an error during evaluation
        message("Something wet wrong with x^2")
        browser()
      }
    else
      {
        return(out)
      }

  }
TestFun("Feng")


###----------------------------------------------------------------------------
### PROFILING
###----------------------------------------------------------------------------

Rprof() # start R profiling

## begin of code blocks
source("loops.R")
## end of code blocks

Rprof(NULL) # end R profiling
summaryRprof() # summary of R profile

###----------------------------------------------------------------------------
### Byte-code compiling
###
### NOTE: Compiler only works with functions
###       Compiler should only used in the non-testing environment
###       because debugging functions are not allowed with compiled functions
###----------------------------------------------------------------------------

## A usual R function
mylapply <- function(X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.list(X))
	X <- as.list(X)
    rval <- vector("list", length(X))
    for(i in seq(along = X))
	rval[i] <- list(FUN(X[[i]], ...))
    names(rval) <- names(X)		  # keep `names' !
    return(rval)
}

## Compiled function (same function but faster)
require("compiler")
mylapplycmp <- cmpfun(mylapply)

## Compare the speed of compiled function (mylapplycmp) and original function
## (mylapply)
x <- 1:10
system.time(for (i in 1:10000) mylapplycmp(x, is.null))
system.time(for (i in 1:10000) mylapply(x, is.null))
