
## creates a wrapper for the matrix in parameter. 
## The wrapper provides $set,$get access methods while $setinv,$getinv access to the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invVal) inv <<- invVal
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieves the cached inverse matrix if exists
## otherwise calculates the inverse of the matrix identified by x parameter

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
