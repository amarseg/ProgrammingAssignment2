## These functions calculate the inverse of a matrix, 
## and save it 
## so the result can be extracted easily without needing to calculate it
## again

## makeCacheMatrix takes the matrix to invert,x, and creates a family of functions
## that allows the manipulation of x and its inverse (inv) by cacheSolve:
##1.- set_mat changes the values of x and inv without having to rerun makeCacheMatrix
##2.- get_mat retrieves the value of the input matrix
##3.- set_inv saves the inverse matrix into the cache
##4.- get_inv fetches the value of the inverse matrix from the cache
##These functions can access the variables defined in makeCacheMatrix even though they are called
##from another environment (cacheSolve). When you define a function inside an environment, it will always
##look in that environment first.


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


## cacheSolve takes the output of makeCacheMatrix. If the inverse of the matrix has been calculated before,
## cacheSolve gets this value from the cache, skipping the computationally heavy step of calculating the inverse.
## If the inverse has not been calculated, the solve function is used and its result is saved into the cache

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
