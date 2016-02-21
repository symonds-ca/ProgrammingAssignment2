## Assignment 2: Lexical Scoping - Caching the Inverse of a Matrix
## 

##  makeVector creates a special "matrix" object, which is a list containing functions to
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the value of the inverse of the matrix
##    - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve returns the inverse matrix using the special matrix object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}