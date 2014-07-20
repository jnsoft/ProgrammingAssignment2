## These functions can be used to make a special "matrix" that can cache it's inverse. 
## The special matrix created by makeCacheMatrix is a list containing a function to 
## set the value of the matrix, get the value of the matrix, set the value of the inverse
## and get the value of the inverse.
## The function cacheSolve tries to retrive and return a cached value of the inverse of 
## a special "matrix". If the cache is empty, it calculates the inverse and stores it 
## in the cache before returning the inverse. 

## makeCacheMatrix creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
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
