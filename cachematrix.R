## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  a <- x
  
  set <- function(y) {
    a <<- y
    i <<- NULL
  }
  
  get <- function() a
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  checkInv <- x$getInv()
  
  if(!is.null(checkInv)){
    message("getting cached data")
    return(CheckInv)
  }
  
  Matrix <- x$get()
  Inverse <- solve(Matrix)
  x$setInv(Inverse)
  Inverse
  
}
