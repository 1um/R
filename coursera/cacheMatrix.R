#Create two functions. First create vector, wich conatian matrix and solve of matrix + method to set or get.
#Second calc solve for CacheMatrix and return it. Or directly return it if solve was calculated before.

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## * set the value of the vector
## * get the value of the vector
## * set the value of the solve
## * get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this funciton calculate solve in makeCacheMatrix and cache it.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  #check in cache
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  s <- solve(data, ...)
  x$setsolve(s)
  s
} 