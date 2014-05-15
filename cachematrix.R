## makeCacheMatrix creates a "cache matrix" object which is
## capable of storing its inverse. the inverse will be
## automatically cached as soon as it is computed once.

## the <<- operator "cause[s] a search to made through parent
## environments for an existing definition of the variable being
## assigned. If such a variable is found ... then its value is
## redefined, otherwise assignment takes place
## in the global environment."


## Returns a list ("cache matrix") containing the matrix, as well
## as functions for 
makeCacheMatrix <- function(x = matrix()) {
  # inv will be the cached inverse of the matrix x
  inv <- NULL

  # set-- sets x and initializes inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # setinv-- sets inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  # get-- returns the matrix x
  get <- function() {
    x
  }
  # getinv-- returns the inverse of x
  getinv <- function() {
    inv
  }

  # return the list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## computes and returns the inverse of x if the inverse is
## not cached, otherwise returns the cached inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  # if the inverse is cached, return it
  if (!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  # otherwise, compute inverse, cache it, and return it
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
