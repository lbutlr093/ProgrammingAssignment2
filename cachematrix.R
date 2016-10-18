## The overall goal of these R functions is to take a matrix as input
## and return the inverse of that matrix. The first time a matrix is
## input, it will calculate the inverse of it and cache the result.
## On subsequent calls, the functions will return the cached inverse.


## This is the constructor class that sets up the makeCacheMatrix object.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- solve      ## setmean -> setmatrix, mean -> matrix
  getinverse <- function() m                     ## getmean -> getmatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This is the function to solve the problem of inversing the makeCacheMatrix
## object. After "solving" by inversing the matrix, the result is then cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}