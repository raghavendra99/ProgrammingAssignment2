## The two functions stated below returns the inverse of given matrix 
## cacheSolve checks if given matrix inverse is already calculated or not
## If the given matrix inverse is already calculated then it's value is returned from makeCacheMatrix

## makeCacheMatrix has the cache of previous matrix inverses

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y=matrix()) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invmat <<- solve()
  getinverse <- function() invmat
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## cacheSolve returns the inverse matrix

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(invmat)
  invmat
        ## invmat is the inverse matrix
}
