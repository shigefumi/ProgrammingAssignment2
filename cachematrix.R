## This functions return inverse of the matrix and cache it
## for later use.

## Return list of sub functions for caching matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setData <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getData <- function() x
    setInverseCache <- function(solve) inverse <<- solve
    getInverseCache <- function() inverse
    list(setData = setData,
         getData = getData,
         setInverseCache = setInverseCache,
         getInverseCache = getInverseCache)
}


## Return the inverse of the matrix created with the above function.
## If there is cached data, return it.
## Else, calcurate inverse and set the inverse as cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverseCache()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    myMatrix <- x$getData()
    inverse <- solve(myMatrix, ...)
    x$setInverseCache(inverse)
    inverse
}
