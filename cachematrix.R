## This file provides a pair of functions that can be used together to calculate the
## inverse of a matrix, and cache the result that can be recalled.

## makeCacheMatrix generates a "maxtix" that can be passed to the cacheSolve function
## and will remember if it has been cached.  i is the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(mean) i <<- mean
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## The cacheSolve function takes a "matrix" created by makeCacheMatrix() and calculates
## the inverse of the matrix, if it has not been calculated previously.  It returns the
## cached value if it has already been calculated.

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}
