## Put comments here that give an overall description of what your
## functions do
## Compute the inverse matrix with cached functionality in R
## Example
## > x <- c(1,2,3,4)
## > dim(x) <- c(2,2)
##> y <- makeCacheMatrix(x)
## > cacheSolve(y)
## result from solve() function
## > cacheSolve(y)
## result from cached data

## Write a short comment describing this function
# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already 
##  been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
