## Functions work together to calculate inverse of a matrix
## while utilizing caches

## Sets a list containing useful values for calculation
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse
  xInv <- NULL
  
  # Create set function for matrix
  set <- function(y) {
    x <<- y
    xInv <- NULL
  }
  
  # Create get function for matrix
  get <- function() x
  
  # Create inverse setting function for matrix
  setInv <- function(inv) xInv <- inv
  
  # Create inverse getting function for matrix
  getInv <- function() xInv
  
  # Return list with data
  list(set = set, get = get, 
       setInv = setInv, getInv = getInv)
}


## Calculates inverse of matrix created above
cacheSolve <- function(x, ...) {
  # Get inverse of x
  xInv <- x$getInv()
  
  # Check status of x
  if (!is.null(xInv)) {
    message("getting cached data")
    # If x is a value return it
    return(xInv)
  }
  # If inverse is not set
  # Get matrix
  matrix <- x$get()
  
  # Get inverse of matrix
  xInv <- solve(matrix)
  
  # Set inverse of matrix
  x$setInv(xInv)
  
  # return inverse
  xInv
}
