## 1. makeCacheMatrix function creates a special "matrix", that is 
##    a list of the following functions:
##       -set the value to the matrix
##       -get the value of the matrix
##       -set the value to the inverse matrix
##       -get the value of the inverse matrix
## ----------------------------------------------------------------
## 2. cacheSolve function returns the inverse of the special 
##    "matrix" created with makeCacheMatrix function. Either the 
##    inverse is calculated on fly or it is retrieved from the cache, 
##    if it has been calculated before  
## ----------------------------------------------------------------


## Return a list of functions (set,get,setInverse,getInverse) for matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <- NULL
  }
  get <-function() x
  setInverse <- function(inverse) inv <<-inverse
  getInverse <-function()inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
