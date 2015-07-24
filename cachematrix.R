

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inverseofx <- NULL
    set <- function(y) {
        x <<- y;
        inverseofx <<- NULL;
    }
    get <- function() x;
    setinv <- function(inv) inverseofx <<- inv;
    getinv <- function() return(inverseofx);
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverseofx <- x$getinv()
    if(!is.null(inverseofx)) {
        message("getting cached data")
        return(inverseofx)
    }
    data <- x$get()
    inverseofx <- solve(data, ...)
    x$setinv(inverseofx)
    return(inverseofx)
}
