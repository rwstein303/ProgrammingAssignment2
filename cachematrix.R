## These functions are used to create an inverse matrix for a square matrix and
## cache it for later use.

## get - return the value of the function
## setinv - create the inverse matrix and cache it
## getinv - retrieve the cached inverted matrix

makeCacheMatrix <- function(x = maxtix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function is used to determine the inverse of a square matrix.  If the
## inverse matrix is cached, it is returned, rather than re-doing the
## inversion.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
