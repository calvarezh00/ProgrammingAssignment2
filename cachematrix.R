## Put comments here that give an overall description of what your

## functions do
#functions that are used to create a special object that stores a matrix and caches its inverse


## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      z <- NULL
      set <- function(y) {
            x <<- y
            z <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) z <<- inverse
      getInverse <- function() z
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}





## Write a short comment describing this function

#Cache the inverse of a matrix: In order to avoid double processing and save resources we return previous resoults.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      z <- x$getInverse()
      if (!is.null(z)) {
            message("getting cached data")
            return(z)
      }
      w <- x$get()
      z <- solve(w, ...)
      x$setInverse(z)
      return(z)
}
