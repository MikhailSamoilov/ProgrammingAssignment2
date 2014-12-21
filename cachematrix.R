## I apologize for my English. For a long time I hadn't a good practice.
## Result: "matrix object"  for caching inverse data
makeCacheMatrix <- function(x = matrix()) {
  # variable to store the inverse.  Default (or start) value = NULL
  inv_data <- NULL
  # "set" is the function to change matrix
  set <- function(data) {
    x <<- data
    inv_data <<- NULL
  }
  
  # "get" is the function to return matrix
  get <- function() { x }

  # "setInverse" - for cacheSolve function
  setInverse <- function(val) {inv_data <<- val}

  # "getInverse" - for getting inverse from cache
  getInverse <- function() {inv_data}

  # Special matrix as result
  list(set = set, get = get,
     setInverse = setInverse, getInverse = getInverse)    
}

## Next Function Result: inversed matrix returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  # load a cached inverse to inv_dat
  inv_data <- x$getInverse()
  if(!is.null(inv_data)) {
    # if the inverse is already cached, then return it.
    message("getting cached inverse")
    return(inv_data)
  }  
  # if the inverse is not cached yet, then calculate the inverse and cache it
  matrx <- x$get()
  inv_data <- solve(matrx, ...)
  x$setInverse(inv_data)
  return(inv_data)
}
