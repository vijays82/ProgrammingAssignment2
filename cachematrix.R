## Pair of functions that cache the inverse of a matrix to avoid costly computations
## functions do

## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector, get the value of the vector, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- solve(x)
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function checks getInverse from the above function to see if it the matrix inverse is already calculated, if so 
## it gets the inverse from the cache, otherwise it calculates the matrix inverse and sets the value of inverse in cache via SetInverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
   if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
   matrix<-x$get()
   inv <- solve(matrix, ...)
   x$setInverse(inv)
   inv
}
