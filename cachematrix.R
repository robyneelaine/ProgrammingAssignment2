## This file contains functions for R Programming assignment 2
## Purpose: create an special matrix object that can cache it's inverse

## This section sets up the solve function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function caches the inverse matrix

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
