## Allows for caching of the inverse of a given matrix.
## Input matricies are expected to be invertible.
## example invertible matrix: matrix(c(2,2,3,2), 2)

## creates a special "matrix", which is really a list containing a function to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns inverse of a "matrix" created by 'makeCacheMatrix'. If inverse
# has already been called, will pull from cache instead of calculating inverse

cacheSolve <- function(x, ...) {
  i <- x$getinv()
      if(!is.null(i)) {
              message("getting cached data")
              return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
