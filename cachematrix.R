## Inverse of a Matrix/Caching the inverse for later use
##
## The following functions are used in conjunction with each other
## makeCacheMatrix cacheSolve

## makeCacheMatrix will take a matrix as an argument and returns a special set
## of objects or functions.  
## The inverse of the matrix is cached and can be retrieved for a later use without having to recalculate
## the inverse again if already set.


makeCacheMatrix <- function(x = matrix()) {
     
     minv <- NULL
     set <- function(y) {
          x <<- y
          minv <<- NULL
     }
     get <- function() x
     setinv <- function(inv) minv <<- inv
     getinv <- function() minv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## CacheSolve takes a list returned by makeCacheMatrix and returns the inverse
## of the matrix X originally passed to MakeCacheMatrix.
## The function uses a list returned from makeCacheMatrix to determine
## whether the matrix inverse needs to be recalculated.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## 'x' is an invertible matrix
     minv <- x$getinv()
     if(!is.null(minv)) {
          message("getting cached data")
          return(minv)
     }
     data <- x$get()
     ## get the inverse of the matrix, 'x'
     minv <- solve(data, ...)
     x$setinv(minv)
     minv
     
     
}

## Example:
# > x<-matrix(c(1,-.25,-.25,1),nrow=2)
# > x
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > myVec <- makeCacheMatrix(x)
# > cacheSolve(myVec)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > 
