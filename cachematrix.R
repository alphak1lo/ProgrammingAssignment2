## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
  #We set the "inv" variable to be NULL. We will use this later
  #In defining "set", we cache both the input matrix z and the inv variable.
  #"get" will return z
  #"setinv" caches the inverse
  #"getinv" returns this inverse
    ##function output is a list containing all four

makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(y) {
    z <<- y
    inv <<- NULL
  }
  get <- function() z
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
  #We set "inv" to be the inverse of the matrix in makeCacheMatrix
  #If inv is not NULL, we return inv
  #If inv is NULL, then we get the matrix (calling it M),
    #We then calculate the inverse of M
    #We then place this inverse in the makeCacheMatrix function
  #Function output is to return inv
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  M <- x$get()
  inv <- solve(M, ...)
  x$setinv(inv)
  inv
}