## Put comments here that give an overall description of what your
## functions do
##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x.solve <- NULL
  # all get and set functions below
  # inverse is initialized as NULL
  set <- function(y) {
    x <<- y
    # re-initialize inverse as NULL on new matrix
    x.solve <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) x.solve <<- solve
  getsolve <- function() x.solve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # check if cached calculation is available
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get the matrix and solve for the inverse, set and return
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


