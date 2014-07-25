## These functions find the inverse of a matrix. They first see if it's been cached

## This makes a "matrix" object with functions to get matrix and inverse

makeCacheMatrix <- function(m) {
    inv <- NULL
    set <- function(n) {
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This solves the inverse of a "matrix" object, first checking if it's already been found and cached. Otherwise, it finds the inverse and caches it.

cachesolve <- function(m, ...) {
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    matrix <- m$get()
    inv <- solve(matrix)
    m$setinverse(inv)
    inv
}
