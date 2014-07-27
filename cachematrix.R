##  Assignment2: Caching the Inverse of a Matrix
##  This is a pair of functions that cache the inverse of a matrix
##  1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cacheSolve retrieve the inverse from the cache.

## makeCacheMatrix returns list of 4 functions
##  1.set: This function creates a special "matrix" and sets inverse(cache) NULL
##  2.get: This function returns created "matrix"
##  3.setSolve: This function caches inverse 
##  4.getSolve: This function returns cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  
   set <- function(y) {
     x <<- y
     inverse <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) inverse <<- Solve
  getSolve <- function() inverse
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
} 


##  cacheSolve returns inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated
##      then return the inverse from the cache
##      else calculate,cache and return inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setSolve(inverse)
  inverse
}


