## makeCacheMatrix takes an object and evaluates if it is an invertible
## squared matrix. If passed the evaluation, it generates a list of functions
## that could be used by cacheSolve to calculate the inverse of that matrix
## and save it in cache. If cacheSolve is called again with the same argument,
## it will return the cached inverse matrix, insted of calculating it again

## Function to generate the object (list of functions) that could be used as 
## argument to the next function. Before doing this, it evaluates if the objects
## passess the conditions to efectively compute the inverse of a matrix. If
## the object does not pass the test, it prompts an error indicating why 
## the operation cannot be done. 

makeCacheMatrix <- function(x = matrix()) {
  if(dim(x)[1] == dim(x)[2] & is.matrix(x) == TRUE & det(x) != 0){
    invmtrx <- NULL
    setm <- function(g) {
      x <<- g
      m <<- NULL
    }
    getm <- function() x
    setinv <- function(inv) invmtrx <<- inv
    getinv <- function() invmtrx
    list(setm = setm, getm = getm,
         setinv = setinv,
         getinv = getinv)
  } else {
    message("Error: Input data is wheather not a squared matrix or it is not invertible")
  }
}

 
## This function will get the special object generated from the previous function
## and calculate the inverse of the matrix stored in that object. It will
## evaluate if the inverse of that matrix was already calculated and stored in cache
## and it will return the cached inverse if so. 

cacheSolve <- function(x, ...)  {
  invmtrx <- x$getinv()
  if(!is.null(invmtrx)) {
    message("Displaying cached inverse")
    return(invmtrx)
  }
  mtrx <- x$getm()
  invmtrx <- solve(mtrx, ...)
  x$setinv(invmtrx)
  invmtrx
}


### To evaluate the function, I recomend using the next expressions

matrix_a <- matrix(1:4, 2,2) # A 2 x 2 filled with numbers from 1 to 4
matrix_b <- matrix(1:9, 3,3) # A 3 x 3 matrix filled with numbers from 1 to 9
matrix_c <- matrix(rnorm(16),4,4) # A 4 x 4 matrix filled with random numbers

aMA <- makeCacheMatrix(matrix_a)
cacheSolve(aMA)
cacheSolve(aMA)

bMA <- makeCacheMatrix(matrix_b) 
cMA <- makeCacheMatrix(matrix_c)
cacheSolve(cMA)
cacheSolve(cMA)
