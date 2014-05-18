## makeCacheMatrix and cacheSolve are two functions used to create a special object that stores a matrix and cache's its inverse.

## makeCacheMatrix takes a matrix and creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" created with makeCacheMatrix. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverser from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets it's value in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
