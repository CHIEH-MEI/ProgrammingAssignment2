makeCacheMatrix <- function(x = matrix()) {
  N <- NULL
  set <- function(y) {
    x <<- y
    N <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) N <<- inverse
  get_inv <- function() N
  list(set = set,
       get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

cacheSolve <- function(x, ...) {
  N <- x$get_inv()
  if (!is.null(N)) {
    return(N)
  }
  data <- x$get()
  N <- solve(data, ...)
  x$set_inv(N)
  N
}