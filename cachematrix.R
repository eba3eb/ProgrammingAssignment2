## Functions work together to calculate inverse of a matrix
## while utilizing caches

## Sets a list containing useful values for calculation
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <- NULL
  }
  get <- function() x
  setInv <- function(inv) xInv <- inv
  getInv <- function() xInv
  list(set = set, get = get, 
       setInv = setInv, getInv = getInv)
}


## Calculates inverse of matrix created above
cacheSolve <- function(x, ...) {
  xInv <- x$getInv()
  if (!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  matrix <- x$get()
  xInv <- solve(matrix)
  x$setInv(xInv)
  xInv
}
