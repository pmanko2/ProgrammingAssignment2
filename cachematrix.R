## Functions to speed up getting inverse of a matrix through
## the use of a caching mechanism

## Takes in a matrix and adds inverse caching functionality to that matrix.
## Inverse is stored in a variable and accessible through getters and setters.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## save matrix into special object
  ## when matrix gets set, the cached inverse is cleared and set to NULL
  ## since it needs to be recalculated for new matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Take in an inversible makeCacheMatrix matrix. Check 
## if there is a cached result of matrix inverse. Return cached result if exists,
## otherwise calculate inverse and save result into cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## cached inverse exists, return instead of recalculating
  if(!is.null(inv)){
    message("getting cached inverse for given matrix")
    return(inv)
  }
  
  ## find inverse of special matrix and cache it using 'setinverse' function
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  return(inv)
}
