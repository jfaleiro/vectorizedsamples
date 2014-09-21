## This R script is composed of two functions to cache previous results of a 
## computationaly intensive operation, calculation of inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # setter of the cached value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # getter of the cached value
    setsolve <- function(solve) m <<- solve # getter of the solved matrix
    getsolve <- function() m # getter of the solved matrix
    # resulting vector of four functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
## assumption: matrix supplied is always invertible

cacheSolve <- function(x, ...) {
    m <- x$getsolve() # checks the last cached data
    if(!is.null(m)) { # if not null, result was calculated before, returns it
        message("getting cached data")
        return(m)
    }
    # solve() was not calculated before
    data <- x$get() # gets original matrix
    m <- solve(data, ...) # calculates inverse
    x$setsolve(m) # caches inverse
    ## Return a matrix that is the inverse of 'x' 
    m
}
