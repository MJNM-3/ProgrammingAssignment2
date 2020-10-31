## Hello, good people! These are two functions that cache the inverse of any matrix,
## well, if said matrix is inversable .

## The makeCacheMatrix creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() (x)
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() (inv)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function
## or retrieve it if it has been calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  M <- x$get()
  inv <- solve(M, ...)
  x$setInverse(inv)
  inv
}
