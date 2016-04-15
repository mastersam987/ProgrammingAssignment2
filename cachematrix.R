## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
##The following functions are used to create a matrix and caches its inverse.

## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL  
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmat) mat <<- invmat
  getinvmatrix <- function() mat
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## This function computes the inverse of the matrix created by above makeCacheMatrix function
## If the inverse is allready calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinvmatrix()
  if(!is.null(mat)){
    message("Getting Cached Data.")
    return(mat)
  }
  datamat <- x$get()
  getim <- solve(datamat, ...)
  x$setinvmatrix(getim)
  getim
}
