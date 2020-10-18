## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix consists of set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL ##initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}   #function to obtain inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##This is used to get the cache data
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
