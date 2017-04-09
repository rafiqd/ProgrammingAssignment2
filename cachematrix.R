## Creates a cacheMatrix object that stores the inverse so subsequent
## calls do not need to recalculate the inverse
## params
##  x: A matrix that you want to create a cacheMatrix out of
##
##  Returns:
##      cacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of a matrix created from makeCacheMatrix()
## caches the inverse so subsequent calls do not need to be cached
## params:
##    x: cacheMatrix object returned from makeCacheMatrix, for which
##       you want to calculate the inverse of
##  ...: extra param arguments to solve()
##
##  Returns:
##      Inverse of the given cacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Getting Cached Data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

