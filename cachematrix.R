## File:CacheMatrix.R
## Author: Hari
## Intent: Cache Matrix is a way to cache inverse of a matrix in a list so the operations if requested again are returned 
## via the cached value and we dont need to calcuate it again. This is helpful if we have a large matrix and require the
## inverse frequenty. This also demostrates use of <<- operator that is used to assign a value to an object in an environment 
## that is different from the current environment. 

## This function allows to store a the cached value of a matrix. They way it achives this is by exposing list with set of 
## functions, get, set, setinversematrix, getinversematrix. The intent here is that the caller cachesolve will be able to
## check is the value is cached, it not it will cache the value of the inverse of the for a matrix. The Set function is to keep
## the matrix provided. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solveresult) m <<- solveresult
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  

}


## This fucntion utilizes the capabilies provided by the makecacheMatrix to check if inverse exists and returns that.
## else it will compute the invese once and set that value back in the makecachematrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
