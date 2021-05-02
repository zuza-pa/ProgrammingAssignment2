## MakeCacheMatrix function have 4 methods
## Set method saves the data of matrix
## Get method returns the data of matrix
## Setinverse method saves the inversed matrix
## Getinverse method returns the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function returns inversed matrix from the cache or will compute the inversed matrix and save it to the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inversed matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
