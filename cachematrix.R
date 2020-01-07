## File contains functions to handle inversion of large matrices.
## For those matrices insersion is a long process. Caching of result
## should improve performance on repeated inversion operations.

## Function creates a "storage" for original matrix and its inversion

makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x)) {
        simpleError("First argument should be matrix")
    }
    cache <- NULL
    set <- function(m) {
        x <<- m;
        cache <<- NULL;
    }
    get <- function() x
    setinv <- function(inversion) cache <<- inversion
    getinv <- function() cache
    list(set = set, get = get, setinversion = setinv, getinversion = getinv)
}


## Function returns inversion for matrix from "storage" object (if available).
## Othervise computes inversion, stores it into object and returns as function
## result.

cacheSolve <- function(x) {
    inv <- x$getinversion() 
    if (is.null(inv)) {
        m <- x$get()
        inv <- solve(m)
        x$setinversion(inv)
    }
    inv
}
