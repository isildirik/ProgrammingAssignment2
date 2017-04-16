## Creates a cached matrix with the given matrix `mat`
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(value){
    mat <<- value
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Creates the inverse of a matrix assuming it is invertable and sets sets the inverse value to the cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x)
  x$setinverse(m)
  m
}
