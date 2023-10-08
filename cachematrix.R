## The first function, `makeCacheMatrix` creates 
## a special "matrix", which is really a list containing a function to 
## 1. set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the mean
## 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## `cacheSolve`: This function computes the inverse of 
## the special "matrix" returned by `makeCacheMatrix` above. 
## We assume that the matrix supplied is always invertible. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache. 
## Computing the inverse of a square matrix can be done with the `solve` function in R. 
## For example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## `cacheSolve_extra`: This function does the same as `cache_Solve` 
## except when the original matrix is singular. 
## R will print a message--"Matrix is singular and does not have an inverse"
## --instead of spitting out an error term.

cacheSolve_extra <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  determinant <- det(data)
  if (determinant != 0) {
    i <- solve(data)
    x$setinverse(i)
    print("Inverse Matrix:")
    print(i)
  } else {
    cat("Matrix is singular and does not have an inverse.")
  }
}

M <- matrix(c(1:4), 2, 2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)
cacheSolve_extra(M1)

M <- matrix(c(1,2,2,4), 2, 2)
M2 <- makeCacheMatrix(M)
cacheSolve(M2)
cacheSolve_extra(M2)
