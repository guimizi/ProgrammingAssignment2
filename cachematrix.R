## Put comments here that give an overall description of what your
## functions do

## Caches a matrix and its inverse, it provides 
## getter/setter functions in order to do that

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## return the inverse of a cached matrix
## if the inverse was already calculated it returns the cached result
## otherwise it calculates the inverse again, store the result in the cache
## and finally return the result
 
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
