## Caching the inverse of a matrix

## Create a special matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

## If the inverse has already been calculated, 
## then function retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    ## Return from cashe
    message("getting cached data")
    return (inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setInverse(inv)
  inv
}
