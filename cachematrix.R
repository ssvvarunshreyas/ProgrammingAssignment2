makeMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL ## initial assignment
  }
  get <- function() x
  setinv <- function(solve) m <<- solve ## solving for matrix
  getinv <- function() m  ## computing new inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## pass x as a mtrix and make sure its det is not 0.

cacheInverse <- function(x, ...) {
  m <- x$getinv()  ## checking cached data
  if(!is.null(m)) {  
    message("getting cached data") ## checking m and finding the presence of cached data
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ## solving the matrix
  x$setinv(m) 
  m
}
