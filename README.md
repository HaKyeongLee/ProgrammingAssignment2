# R Programming - Caching Inverse Matrix

##### *Ha Kyeong Lee*

##### *8 October 2023*

##### 

### Assignment

Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

### Caching the Inverse of a Vector

The first function, `makeCacheMatrix` creates a special "matrix", which is really a list containing a function to

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the mean
4.  get the value of the mean

```         
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
```

`cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. **we assume that the matrix supplied is always invertible.** If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache. Computing the inverse of a square matrix can be done with the `solve` function in R. For example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.

``` {style="gray"}
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
```

`cacheSolve_extra`: This function does the same as `cache_Solve` except when the original matrix is singular. **R will print a message--"Matrix is singular and does not have an inverse"--instead of spitting out an error term.**

``` {style="gray"}
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
```

### Testing

#### Example 1 with an Invertable Matrix

``` {style="gray"}
M <- matrix(c(1:4), 2, 2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)
```

```         
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
```

``` {style="gray"}
cacheSolve_extra(M1)
```

```         
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
```

#### Example 2 with a Singular Matrix

``` {style="gray"}
M <- matrix(c(1,2,2,4), 2, 2)
M2 <- makeCacheMatrix(M)
cacheSolve(M2)
```

```         
Error in solve.default(data, ...) :
Lapack routine dgesv: system is exactly singular: U[2,2] = 0
```

``` {style="gray"}
cacheSolve_extra(M2)
```

```         
Matrix is singular and does not have an inverse.
```
