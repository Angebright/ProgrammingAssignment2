## These two functions work together to create a "special" matrix object
## that can cache its own inverse, avoiding the need to recompute an
## expensive matrix inversion if it has already been calculated for the
## current matrix contents.

## makeCacheMatrix creates a special "matrix" object that can cache its
## inverse. It returns a list of functions to set/get the matrix and
## set/get its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the
## inverse from the cache instead of recomputing it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
