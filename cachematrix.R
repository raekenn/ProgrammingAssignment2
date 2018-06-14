## makeCacheMatric and cacheColve will create and cache
## the inverse of a given matrix

## This function will store the matrix along with the cached
## inverse, if computed

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## declare set function
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## declare get function
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This funciton will return the inverse (either cached
## and already computed or newly computed)

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    ## return cached inverse
    if(!is.null(i)) {
        message('returning cached inverse')
        return(i)
    } ##else return newly calculated inverse and cache inverse
    else {
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        return(i)
    }
}
