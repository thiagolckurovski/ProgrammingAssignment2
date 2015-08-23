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
  # Initialize cached inverse with NULL
  i <- NULL

  # Set the new matrix, initialize cached
  # inverse with NULL
  # Need <<- to set internal scope
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # Return matrix
  get <- function() x
  
  # Internal function: set cached matrix
  # Need <<- to set internal scope
  setinv <- function(inv) i <<- inv
  
  # Internal function: get cached matrix
  getinv <- function() i
  
  # Return list of functions required to
  # access internal cached data,
  # plus set and get interfaces
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
  # Obtain cached data
  i <- x$getinv()
  
  # If the cached data is not NULL
  # return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise generate a cached
  # inverse using solve(), store
  # it and return it
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
