## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed,
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
