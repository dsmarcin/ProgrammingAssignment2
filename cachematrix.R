## These two functions will calculate the inverse of a matrix.  First, you will
## initialize the matrix with makeCacheMatrix.  That function saves the matrix
## and also saves functions for computing inverses. They are not called within
## that function, though.  cacheSolve will look at the elements of the result
## of makeCacheMatrix to see if it needs to compute the inverse or if it can
## pull it from the cache.

## makeCacheMatrix takes a matrix as its input and saves the matrix along with
## three functions for computing or retrieving the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinv <- function(solve) v <<- solve
  getinv <- function() v
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve takes the result of makeCacheMatrix and first looks for the 
## inverse of the matrix.  If it does not exist, then it computes it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  v <- x$getinv()
  ## This upcoming if statement will trigger if v exists, i.e. if there is
  ## a non-null entry in the getinv field of the result of makeCacheMatrix
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
    ## The function ends here if not null, saving us from computing the inverse.
  }
  data <- x$get()
  v <- solve(data, ...)
  ## The previous line computes the inverse
  x$setinv(v)
  ## The previous line sets the inverse inside of x, so that if we need it
  ## again, it's already there.
  v
  
}
