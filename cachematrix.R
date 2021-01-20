## These two functions will allow saving the result of the inverse of an matrix
## in the cache memory, thus preventing r from performing the calculation
## each time this information is requested,thus saving time and computational cost.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to: set the value of the matrix, get the value of the 
## matrix set the value of the inverse and get the value of the inverse

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

## Write a short comment describing this function
## it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinverse function.

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