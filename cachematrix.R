## This script caches the inverse of a matrix

## This function creates a special matrix that enables
## other parts of the script to get or set the matrix
## and also get or set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks to see if the inverse has
## already been created and if so return the cached version
## - if the cached version has not been created then
## calculate the inverse and save it to the cached version
## A message is included to tell us when the cached version 
## is being accessed

cacheSolve <- function(x, ...) {
        
  i <- x$getInverse()
  if(!is.null(i)){
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
