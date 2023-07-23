## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## pass x as a square invertible matrix.
  ## return: a list containing functions to
  ## set the matrix
  ## get the matrix
  ## set the inverse
  ## get the inverse
  
  minv = NULL
  set = function(y) {
    # `<<-` used to assign a value to an object in a different environment
    x <<- y
    minv <<- NULL
  }
  get = function() x
  setminv = function(inverse) minv <<- inverse 
  getminv = function() minv
  list(set=set, get=get, setminv=setminv, getminv=getminv)
}

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## result of this function is the inverse of the original matrix input to makeCacheMatrix()
  
  cinv = x$getminv()
  
  # if the inverse has already been calculated
  if (!is.null(minv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(minv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  minv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setminv(minv)
  
  return(cinv)
}

makevector<- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}