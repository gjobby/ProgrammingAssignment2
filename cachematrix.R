
makeCacheMatrix <- function(x = matrix()) {  ##Create the cache matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}
cacheSolve <- function(x, ...) {
  i <- x$geti()
  if(!is.null(i)) {       ## Check if inverse has been cached
    message("getting cached data") ## Get Cached value
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)   ## Inverse the matrix
  x$seti(i)
  
  ## Return the inverse
  i
}