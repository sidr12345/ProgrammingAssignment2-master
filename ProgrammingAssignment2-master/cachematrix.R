## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function creates a object for the "cacheSolve" function to turn into the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

## this function calculutes the inverse of the object which was generated from makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("the inverse is has been cached")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  return(inv)
}
