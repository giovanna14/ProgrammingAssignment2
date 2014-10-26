## Put comments here that give an overall description of what your
## functions do
# This pair of functions creates an object that
# stores an (assumed invertible) matrix 'x' and caches its inverse. 
# The inverse of the matrix is calculated only if it is not
# yet on cache.

## Write a short comment describing this function
# The function makeCacheMatrix() creates a special matrix 
# that is actually a list of four functions that:
# 1. set the value of the matrix 'x'; 
# 2. get the value of the matrix;
# 3. set the value of the inverse of the matrix;
# 4. get the valure of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
    get <- function() x
    setinvm <- function(inverse) invm <<- inverse
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## Write a short comment describing this function
# The function cacheSolve() -applied to the output of makeCacheMatrix()- 
# checks whether the inverse of the matrix 'x'
# is already on cache. If yes, it returns it with a message.
# If not, it proceeds to calculate the inverse of 'x', it
# caches its value and returns it.

cacheSolve <- function(x, ...) {
    invm <- x$getinvm()                  # reads the cache into invm
        ## Return a matrix that is the inverse of 'x'
    if (!is.null(invm)) {                # check for existing inverse of 'x'
          message("getting cached data") # return a message
          return(invm)                   # return the inverse of 'x' from cache
    }
    data <- x$get()                      # retrieve the matrix 'x'
    invm <- solve(data, ...)             # calculate the inverse of 'x'
    x$setinvm(invm)                      # caches the inverse of 'x'
    invm                                 # return the inverse of 'x'
}
