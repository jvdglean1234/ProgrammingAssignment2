## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##init inv as null
  inv <- NULL
  
  ##set function for assigning the input matrix
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  
  ##get function for returning matrix
  get <- function() x
  
  ##compute the inverse of matrix
  setinv <- function(solve) inv <<- solve
  
  ##return inverse value
  getinv <- function() inv
    
  ##list function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ##Get the inverse value from cacheMatrix object
  inv <- x$getinv()
  
  ##Return inverse value if that is cached before
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  ##If it is not cached before, retrieve invertible matrix
  mat <- x$get()
  ##Compute the inverse value
  inv <- solve(mat, ...)
  ##Assign the computed inverse into cacheMatrix object
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
