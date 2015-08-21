## Creates a special "matrix", 
## which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the cached matrix
## get the value of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(cmatrix) m <<- cmatrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## This function computes the inverse matrix of 
## a special "matrix" matrix retrived by the function above.
## To perform the calculation, this function first
## checks if the spetial "matrix" has already a inverse matrix
## calculated for x parameter. If none value is returned
## cacheSolve perform calculation and store the result in
## the cached matrix to be retrived in the future, otherwise
## retrives the value of the cached matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m 
  ## Return a matrix that is the inverse of 'x'
}
