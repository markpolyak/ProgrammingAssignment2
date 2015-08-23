## The functions below define a special type of a matrix that can cache its inverse.
## This can be handy when working with large matricies and an inverse of the same matrix
## needs to be calculated several times (e.g. in a loop).
##
## Example:
## m <- matrix(rnorm(9), 3, 3)
## mc <- makeCacheMatrix(m)
## for (i in 1:2) print(cacheSolve(mc))
##

## Creates a special type of a matrix that can cache its inverse. Argument x is an ordinary R matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted) inv <<- inverted
  getinv <- function() inv
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## Calculate inverse of a special matrix x. Returns cached value if the inverse was already calculated earlier.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) { # check if the inverse was cached earlier
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # inverse was not calculated earlier so calculate it now
  inv <- solve(data, ...)
  x$setinv(inv) # cache newly calculated inverse
  inv
}
