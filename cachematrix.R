## makeCacheMatrix() and cacheSolve() work to cache a matrix inverse.
## This reduces computation time and memory space
## See Wiki page "Example Showing How makeCacheMatrix() and cacheSolve() 
## work to cache a matrix inverse" at the repository
## repository: https://github.com/shankar4/ProgrammingAssignment2


## make a list that holds four functions for caching a matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinv <- function(inv) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## obtain matrix inverse. Use cached inverse if already computed

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
