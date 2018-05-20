## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set_mat <- function(y){
    x <<- y
    inv <<- NULL}
  get_mat <- function() x
  set_inv <- function(inv_mat) inv <<- inv_mat 
  get_inv <- function() inv
  list(set_mat = set_mat, get_mat = get_mat, set_inv=set_inv, get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)){
    message('getting from cache')
    return(inv)
  }
  y <- x$get_mat()
  inv <- solve(y)
  x$set_inv(inv)
  x$get_inv()
}
