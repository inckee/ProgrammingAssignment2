## This function will let us cache the inverse of a matrix 
## so that it does not need to be re-evaluated each time

## The first part creates an object that stores a list of functions
## so that these functions can be called up using the $ operator

makeCacheMatrix <- function(x = matrix()) {
  solved_matrix <- NULL
  made <- function(y) {
    solved_matrix <<- NULL
    x <<- y
  }
  get <- function() {x}
  set_inverse <- function(solve) {solved_matrix <<- solve}
  get_inverse <- function() solved_matrix
  list(made = made, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function will return a matrix that is the inverse 
## of our original matrix, if the inverse is already present
## then it will not calculate the inverse but rather bring
## the cached data

cacheSolve <- function(x, ...) {
  solved_matrix <- x$get_inverse()
  if(!is.null(solved_matrix)) {
    message("getting cached data")
    return(solved_matrix)
  }
  data <- x$get()
  solved_matrix <- solve(data, ...)
  x$set_inverse(solved_matrix)
  solved_matrix
}