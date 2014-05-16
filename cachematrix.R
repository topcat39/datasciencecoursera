## Caches an Inverted Matrix for future use
## An initial matrix must be created first, using
##    makeCacheMatrix


## makeCacheMatrix - creates a Matrix that acts like a class
##    sets/gets the value of the matrix
##    sets/gets the value of the inverted matrix
##

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve solves a matrix (return the inverse)
## if matrix previously solved, simply return the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
