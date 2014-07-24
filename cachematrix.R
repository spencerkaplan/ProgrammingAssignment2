## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
