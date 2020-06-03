## A pair of functions that checks and sets the cache of inverse matrix.
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invers_e<-NULL
  set<-function(y){
    x<<-y
    invers_e<<-NULL
  }
  get <- function() x
  setinverse <- function(invers__e) {invers_e<<-invers__e}
  getinverse <- function() invers_e  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# returns the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invers_e<-x$getinverse()
  if(!is.null(invers_e)){
    message("getting cached inverse")
    return(invers_e)
  }
  data<-x$get()
  invers_e <- solve(data,...)
  x$setinverse(invers_e)
  invers_e
}