## The functions in this R source file make it possible to create a
## special matrix that can cache its inverse, calculating it lazily
## and only once

makeCacheMatrix <- function(x = matrix()) {
    ## Makes a matrix that can cache its inverse. 
    ##
    ## This function creates a special matrix object
    ## that can cache its inverse.
    ## Re-setting the matrix data also resets the inverse,
    ## making it necessary to recalculate it.
    ##
    ## Args:
    ##   x: The initial value for the matrix. Default is an
    ##      empty matrix.
    
    inverse <- NULL
    
    set <- function(y) {
        ## Sets a new value for this matrix.
        ##
        ## Sets a new value for this matrix,
        ## and resets the cached inverse, if it was calculated.
        ##
        ## Args:
        ##   y: The new value for this matrix.
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        ## Gets the actual matrix value.
        x
    }
    
    setinverse <- function(newinverse) {
        ## Sets the inverse for this matrix
        ##
        ## Args:
        ##   newinverse: The new value for the
        ##               inverse of this matrix.
        inverse <<- newinverse
    }
    
    getinverse <- function() {
        ## Gets the inverse for this matrix
        inverse
    }
    
    # Returns a list with the four functions defined
    # for this special matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Calculates the inverse for the special matrix x, if needed.
    ##
    ## This function computes the inverse of the special matrix
    ## returned by the function makeCacheMatrix.
    ## If the inverse has already been calculated 
    ## (and the matrix has not changed), then cachesolve retrieves
    ## the inverse from the cache.
    ##
    ## Args:
    ##   x: The special matrix object.
    ##   ...: Other arguments that will be passed to
    ##        the solve function.
    
    inverse <- x$getinverse()
    
    # Since we have a cached inverse, we'll just return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # There is no cached inverse, so we calculate and set it.
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    # Returning the newly calculated inverse
    inverse
}
