##The functions makeCacheMatrix and cacheSolve are utilized to cache the 
#inverse of a matrix.

## The function makeCacheMatrix creaste a special object "matrix" and 
#returns a list containing 4 different functions: ##set,get,setInverse and getInverse.


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


### The function, cacheSolve, calculates the inverse of the matrix created by 
#the above function, makeCacheMatrix. It first checks the value of the Inverse of 
#the matrix: if the value is already been calculated, it gets the inverse of the matrix from the cache. In this case computation is  avoided. Otherwise, this function (cacheSolve) calculates the inverse of the 
#matrix and sets teh value of inverse in the cache via the setmean function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
