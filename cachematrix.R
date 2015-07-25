## Put comments here that give an overall description of what your
## functions do

## Creates a list of variables used for testing the
## existence of a pre calculated matrix inverse
## to be called in a separate function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)  
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## x$getsolve returns a value from the cache function
  m <- x$getsolve()
  ## a matrix times its inverse is the identity function
  ## a matrix times the identity returns the same matrix
  ## 'if' tests to determine whether the cached matrix
  ## is actually an inverse of 'x'
  if(((x %*% m) %*% x) == x) {
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
}