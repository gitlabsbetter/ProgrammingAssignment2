## The following functions create a matrix and then returns the inverse from cache, if previously calculated.

## makeCacheMatrix function creates a matrix from passed argument values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve will return the inverse of the matrix created via makeCacheMatrix.
## If the inverse has not been calculated it will be cached; then subsequent calls will retrieve the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("returning cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  return(inv)
}
