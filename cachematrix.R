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
