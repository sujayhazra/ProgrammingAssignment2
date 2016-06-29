# See README.MD for instructions on running the code and output from it

# makeCacheMatrix is a function that returns a list of functions. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   set the cached value (inverse of the matrix)
# * getInverse     get the cached value (inverse of the matrix)
#
# Notes:
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its 
# inverse. It is assumed that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or NULL if nothing is cached, initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # returns the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the inverse of the matrix
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of the matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
        # get the cached value of the inverse matrix
        inverse <- x$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in the cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$cacheInverse(inverse)
        
        # return the inverse
        inverse
}