## These functions calculate the inverse of a square matrix
## They cache this result so that a new call does not recalculate the matrix

## The input x is a square invertible matrix
## Returns a list containing the functions to:
## Get the input matrix
## Set the values of the inverse matrix
## Get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The input x is the list returned by makeCacheMatrix
## Returns the inverse matrix of the matrix input to the input x
## Checks first to see if the result has been stored previously in the cache
## If so, it returns the previous result instead of recalculating it
## If not, it calculates the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}