## Matrix object with inverse calculation caching
##
## Provide two functions:
## - makeCacheMatrix creates a matrix with inverse
##   caching.
## - cacheSolve loads the inverse from cache or
##   calculates it as needed.

## Receive a matrix, return a matrix with inverse caching
##
## Build a list with two interface functions:
## - get returns the internal matrix
## - set sets the matrix to another value
## and two internal ones:
## - getinv returns the cached inverse
## - setinv sets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Receive a cacheMatrix list, return its inverse
##
## Either returns the cached inverse, if it has
## already been calculated, or calculates the inverse,
## stores it in the cache and returns the result

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
