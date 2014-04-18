## This is the programming assignment 2 for the R programming course (session April 4 2014)
## Author: Paul Balm
## 
## The below function makeCacheMatrix creates an object that holds a matrix and caches the inverse
## of the matrix. The object returns by makeCacheMatrix is really a list holding a number of
## functions that can be used to access the original matrix, and the cached value of the matrix
## inverse. Once the inverse is calculated, it can be set in the cache using the setinverse
## function, and retrieved using the getinverse function. The usage of the object returned by
## makeCacheMatrix is made simple by the cacheSolve function defined next. See comments on that
## method for further details.

## Create an object (a list) that can be used to hold a matrix and cache the inverse of this matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse

    getinverse <- function() inv
    
    list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}

## This fuction returns the inverse of a matrix, when given the list produced with makeCacheMatrix.
## The method first checks if the list already has the inverse of the matrix cached. If it does,
## it returns that. Otherwise, it will calcule the inverse, set the result into the cache and return
## the inverse.
## 
## Example usage:
## > set.seed(1)
## > m <- matrix(rnorm(16), 4, 4)
## > mc <- makeCacheMatrix(m)
## > cacheSolve(mc)
##   ... the inverse is calculate and printed ... 
## > cacheSolve(mc)
##   ... the inverse is taken from the cache, not recalculated, and printed ...
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
