## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function makeCacheMatrix creates a special matrix. It is a list that contains the following functions
# $set the value of the matrix
# $get the value of the matrix
# $setInv - set the matrix inverse
# $getInv - get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

# The function cacheSolve calculates the inverse of the special matrix.
# But it checks if the inverse matrix already exists.
# IF the $getInv is NULL, it calculates and sets the inverse matrix through the $setInv command

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
  
}
