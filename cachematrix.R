## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:

# the function saves a matrix and gives the possibility to cache the inverse of 
# of that matrix. It returns a list of four functions which allow setting and getting
# the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y           # loop in case inverse equals to 0
    inverse <<- NULL 
  }
  
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,            # the list is filled with elements
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# the function cacheSolve returns the inverse of the matrix that is stored
# in the list that is passed in argument x (and has been created by the 
# makeCacheMatrix function).
# if the inverse has not yet been computed it is calculated and then stored in 
# the makeCacheMatrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {     # function only applies if the inverse is not 0
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
