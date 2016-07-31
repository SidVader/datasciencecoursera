## The pair of functions makeCacheMatrix and cacheSolve
## cache the inverse of the matrix and always to retrieve
## the inverse until the original matrix is unchanged.


## The function makeCacheMatrix creates a special "Matrix" object
## that can help to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
##Initially the inverse value is NULL
    inv <- NULL
## A flag is set to verify is the matrix unchanged
    flag <- TRUE
    set <- function(y){
        x <<- y
        inv <<- NULL
        flag <<- TRUE
    }
    get <- function() x
## The setinverse cache the inverse in inv
    setinverse <- function(value) {
      inv <<- value
      flag <<- FALSE
    }
    getinverse <- function() inv
    matchange <- function() flag
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse,
    matchange = matchange)
}


## This function solves for a matrix's inverse if it hasn't been cached previously.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    isMatChanged <- x$matchange()
## isMatChanged is to verify whether the matrix has changed.
    if(!is.null(inv) && !isMatChanged) {
        message("getting cached data")
        return(inv)
    }
## If the matrix has changed or the cached value is empty
## than the inverse is computed using solve().
    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}
