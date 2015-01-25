## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # cached initialized to NULL (cache not set)
        s <- NULL
        # set the underlying matrix
        set <- function(y) {
                #  set the matrix and reset the cache
                x <<- y
                s <<- NULL
        }
        # get the underlying matrix
        get <- function() x
        # set (i.e. cache) the sovled inverse
        setSolve <- function(solved) s <<- solved
        # get the solved inverse (will return NULL if cache has not been set)
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Return a matrix that is the inverse of 'x'
## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        # check if the inverse data is cached (not NULL)
        # if not cached, calculate the inverse and cache it
        # in both cases, inform the user via the message function
        if(!is.null(s)) {
                message("getting cached data")
        }
        else {
                message("calculating and setting cached data")
                # get the underlying matrix data, solve, and cache
                data <- x$get()
                s <- solve(data, ...)
                x$setSolve(s)
        }
        ## Return s (the matrix that is the solved inverse of 'x')
        s
}
