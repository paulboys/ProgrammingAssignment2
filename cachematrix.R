## This is a function to calculate the inverse of a matrix: the funciton checks 
## to see if the inverse has already been calculated and cached. If so it 
## retrieves the inverse from the cache. Otherwise it calculates the inverse.

## This is a function that creates a list:
#1)sets the value of the matrix
#2)gets the value of the matrix
#3)sets the value of the inverse
#4)gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}


## This function computes the inverse of  matrix returned by makeCacheMatrix; 
## if the inverse has been calcualted already, cacheSolve retrieves the inverse 
## from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
