## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(mat) m <<- mat
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {        # If there is already a cached matrix, print it
        message("Getting the inverse for the input matrix...")
        return(m)
  }
  
  # If m is null, then determine the inverse for the input matrix
  
  data <- x$get()     # Reading in the original matrix
  m <- solve(data)    # Calculating the inverse for the input matrix
  x$setInverse(m)     # Sending the inverse to the cache
  m                   # Returning the inverse
  
}
