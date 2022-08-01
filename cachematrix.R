## These functions cache the inverse of a matrix and
## recall that cached value when required


## makeCacheMatrix creates a special matrix that is able
## to cache the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  # Create special matrix from matrix
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) n <<- solve
  getinverse <- function() n
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The cacheSolve function detrmines if the inverse of the special matrix
## returned by makeCacheMatrix has bee cached.

## If the inverse has been cached and the matrix did not change the cached
## value is returned, else the inverse is calculated

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
  
  
}