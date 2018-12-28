## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has ## not changed), then the cachesolve should retrieve the inverse from the cache.

# This function creates a special "matrix" object
# that can cache its inverse.
#
# Args:
#   x: The matrix to make cachable
#
# Returns:
#   A list of four excuatble functions

makeCacheMatrix <- function(x = matrix()) {
  inverse_ <- NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_ <<- inverse
  getInverse <- function() inverse_
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function creates a special "matrix" object that can
# cache its inverse.
#
# Args:
#   x: The "cacheable matrix"
#
# Returns:
#   Inverse Matrix of the "special matrix"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_ <- x$getInverse()
  if (!is.null(inverse_)) {
    message("getting cached data")
    return(inverse_)
  }
  data <- x$get()
  inverse_ <- solve(data)
  x$setInverse(inverse_)
  inverse_
}
