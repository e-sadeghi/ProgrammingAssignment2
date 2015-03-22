
##The function, makeCacheMatrix creates a list containing 4 functions to:

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  l <- NULL
  set <- function(y) {
    x <<- y
    l <<- NULL
  }
  get <- function() x
  setinv <- function(inv) l <<- inv
  getinv <- function() l
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##The cacheSolve function calculates the inverse of the matrix created with the above function
##if the inverse has already been in the cache, it returns the inverse from cache
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache by the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  l <- x$getinv()
  if(!is.null(l)) {
    message("getting cached data")
    return(l)
  }
  data <- x$get()
  l <-solve(data, ...)
  x$setinv(l)
  l
}
