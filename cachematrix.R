## The following 2 functions make the computer to check if it has done calculation of 
## the inverse of a matrix before. If yes, then the computer doesn't have to re-calculate 
## again, it can retrieve from cache memory. 

## The function makeCacheMatrix creates a list of special functions, so that the computation 
## of the inverse of a matrix can be stored in cache. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The function cacheSolve will return the inverse of a matrix. The matrix should be 
## pre-processed with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}