## File contains functions to handle inverse of large matrices.
## For those matrices inverse computing is a long process. Result
## of computation should be stored into cache to improve performance.

## Function creates a "storage" for original matrix x and its inversion
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


## Function returns inverse for matrix from "storage" object x (if available).
## Othervise computes inverse, stores it into object and returns as function
## result. Dot-dot-dot arguments are passed to solve function as is.

cacheSolve <- function(x, ...) {
    inv <- x$getinversion() 
    if (is.null(inv)) {
        m <- x$get()
        inv <- solve(m, ...)
        x$setinversion(inv)
    }
    inv
}
