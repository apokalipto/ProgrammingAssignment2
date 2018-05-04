## Calculate the inverse of a matrix. Use a cache structure to optimize computations
## 

## Construct that stores a matrix and its inverse. If the inverse is not stored NULL is returned

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(toCache) inv <<- toCache
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns the inverse of a matrix. The result is cached.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)){
      message("Cached Inverse")
      return(inverse);
    }
    
    original <- x$get()
    inverse <- solve(original)
    x$setInverse(inverse)
    
    inverse
}
