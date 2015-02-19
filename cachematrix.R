## The functions compute an inverse of a matrix and cache its inverse

## Creates a matrix object that 
## is a list of functions (get, set, getinv, setinv) 

makeCacheMatrix <- function(x = matrix()) {
  xi <- NA
  set <- function(y) {
    x <<- y
    xi <<- NA
  }
  get <- function() x
  setinv <- function(inv) xi <<- inv
  getinv <- function() xi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function checks if the inverse of the matrix object has been calculated. If it has been, 
## gets the cached inverse matrix. If it has not been, the function gets the matrix, calculates its inverse and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xi <- x$getinv()
  if(!is.na(xi[1])) {
    message("getting cached inverse matrix")
    return(xi)
  }
  
  data <- x$get()
  xi <- solve(data, ...)
  x$setinv(xi)
  xi
  
}
