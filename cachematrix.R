## Utility functions for bundling a matrix together with its inverse.  
## The inverse will be computed once when demanded and then cached.

## Construct a matrix wrapper object containing the given matrix
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  
  set <- function(new_matrix) {
    x <<- new_matrix
    cached_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) cached_inverse <<- inv
  get_inverse <- function() cached_inverse
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}

## Given a matrix wrapper object built with makeCacheMatrix, return the inverse
## of the matrix.  The inverse will be computed at most once.
cacheSolve <- function(x, ...) {
  cached_inverse <- x$get_inverse()
  if(!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }
  x.matrix <- x$get()
  cached_inverse <- solve(x.matrix)
  x$set_inverse(cached_inverse)
  cached_inverse
}
