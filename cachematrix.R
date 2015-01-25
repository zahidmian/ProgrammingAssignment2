## Put comments here that give an overall description of what your
## functions do

#function, makeVector creates a special "vector", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # define set, get, setInverse, and getInverse functions
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  
  # construct command list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
