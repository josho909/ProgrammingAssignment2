## These functions set a matrix and return the previously set inverse_matrix
## or if none has been set then it returns the inverse and saves it in cache

## Makes list of functions for setting and returning the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(solve) inverse_matrix <<- solve
  get_inverse_matrix <- function() inverse_matrix
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## Solves for inverse matrix if no inverse_matrix is found

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$get_inverse_matrix()
        if(!is.null(inverse_matrix)) {
          message('getting cached data')
          return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$set_inverse_matrix(inverse_matrix)
        inverse_matrix
}
