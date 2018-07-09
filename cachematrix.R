##  a pair of functions that cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y){
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv_m <<- inverse
    getInv <- function() inv_m
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed) =>
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getInv()
    if (!is.null(inv_m)){
        message("getting cached data")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setInv(inv_m)
    inv_m
}
