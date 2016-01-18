## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## computing it repeatedly.
## Two functions are created below to help us acheive this.

## Function 1: "makeCacheMatrix" This function creates a special
## matrix object that can cache its inverse and 4 functions are
## created as a special list in this function:
## set: set the value of the matrix
## get: get the value of the matrix
## setinverse: set the inverse of the matrix
## getinverse: get the inverse of the matrix

makeCacheMatrixs <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
