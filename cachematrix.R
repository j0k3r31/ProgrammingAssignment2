## Put comments here that give an overall description of what your
## functions do
## to write 2 functions to cache the inverse of a matrix
## Write a short comment describing this function

## makeCacheMatrix is a function that creates an object that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
  
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(inverterse) invert <<- inverterse
  getinvert <- function() invert
  list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## Write a short comment describing this function

## cacheSolve function computes the inverse of the object returned by the function above.
## if the matrix is not changed when the inverse has been computed then cache solve function returns its invers from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting matix cached")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  invert
}
}
