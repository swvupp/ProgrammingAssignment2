## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## set is a function that changes the vector stored in the main function.  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
##  get is a function that returns the vector x stored in the main function.
  get <- function() x

## setinverse is a function that stores the inverse of the matrix in the value m  
    
  setinverse <- function(solve) {
  m <<- solve
  }  

## getinverse is a function that returns the value m
  
  getinverse <- function() m

list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## Function "cacheSolve" computes the inverse of the special "matrix" (which is the input of 
## cachematrix) returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the 
## cache. If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
## m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}