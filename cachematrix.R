## Together these functions enable quicker results when calculating the inverses of matrices.
## makeCacheMatrix creates a special object from the matrix that can be cached.
## cacheSolve calculates the inverse of the object and caches it for future reference.

## Given a matrix x, this function creates a special object that can be used to cache the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function calculates the inverse of a special matrix object, caches it, and returns the cached
## value when subsequently called.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message("Getting cached data")
        return (inv)
        }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
