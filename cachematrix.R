## Both functions are the way of obtaining a new type of matrix with cache
## info about its inverse, and the way of retrieve that info.

## makeCacheMatrix makes a Matrix with cache information about their inverse

makeCacheMatrix <- function(M = matrix()) {
    IM <- NULL
    set <- function(G) {
        M <<- G
        IM <<- NULL
    }
    get <- function() M
    setinverse <- function(inverse) IM <<- inverse
    getinverse <- function() IM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve provide cache information about the inverse matrix
## or solve, store and provide the inverse matrix

cacheSolve <- function(M, ...) {
    inverse <- M$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- M$get()
    inverse <- solve(data, ...)
    M$setinverse(inverse)
    inverse
}
