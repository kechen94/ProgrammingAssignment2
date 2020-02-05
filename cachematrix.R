## Put comments here that give an overall description of what your
## functions do
## A matrix is created by function makeCacheMatrix(), then the inverse of matrix was calculated or founf by cacheSolve ()

## Write a short comment describing this function
## This function creates a  "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)   
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  myMatrix <- x$get()
  inv <- solve(myMatrix, ...)
  x$setInv(inv)
  inv
}
