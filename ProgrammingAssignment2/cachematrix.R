## The two functions stated below returns the inverse of given matrix 
## cacheSolve checks if given matrix inverse is already calculated or not
## If the given matrix inverse is already calculated then it's value is returned from makeCacheMatrix

## makeCacheMatrix has the cache of previous matrix inverses

makeCacheMatrix <- function(x = matrix()) {
  ##initialize invmat to null
  invmat <- NULL
  ##set function caches value
  set <- function(y=matrix()) {
    x <<- y
    ##changes invmat to null if there is a change in matrix
    invmat <<- NULL
  }
  ##gets value of inverse
  get <- function() x
  ##calculates the inverse with solve
  setinverse <- function(solve) invmat <<- solve()
  ##gets the inverse
  getinverse <- function() invmat
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## cacheSolve returns the inverse matrix

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  ##if inverse exists gets it
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  ##if inverse is not there it calcuates inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(invmat)
  invmat
  ## invmat is the inverse matrix
}

