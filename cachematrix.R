## Caching results is good becuase you can look them up instead of re-computing
## makeCacheMatrix creates the inputs to cacheSolve () containing a list of functions
## that set / get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## use the <<- operator to assign a value to an object in a different environment
    ## to the current
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function (inverse)  inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolev returns a matrix that is the inv of 'x' (the output of makeCacheMatrix)

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## if the inv has already been calculated retrieve from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise, calculate the inv
  data <- x$get()
  inv <- solve(data)
  ##reset the value of the inv in the cache
  x$setinverse(inv)
  return(inv)
        
}
