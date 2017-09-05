## Caching the Inverse of a Matrix:
## Matrix Inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.  
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and acches its inverse

## The first function, makeCacheMatrix, creates a special “CacheMatrix”, which
## creates a special “matrix” object that can cache its inverse, and then
## set the value of the CacheMatrix
## get the value of the CacheMatrix
## set the value of the CacheMatrix
## get the value of the CacheMatrix
## The CacheMatrix is the object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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
