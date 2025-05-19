## These two functions serve the overall purpose of finding the inverse of a matrix. The first function caches the values of the input matrix so that if 
## the matrix needs to be recalculated, it can just use the cached values rather than having to run the function of the entire matrix again. The second function 
## actually does the recalculations using the cached data if necessary. This is achieved through the usage of lexical scoping.

## This function makeCacheMatrix creates a vector under x that has a function to set and get the values of the inverse and the vector.

makeCacheMatrix <- function(x = matrix()) {
  # Initializes NULL for inverse
  slv <- NULL
  
  #sets and gets for matrix and inverse
  set <- function(y) {
    x <<- y
    slv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) slv <<- solve
  getinverse <- function() slv
  
  # returns a list with these values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes and displays the matrix with inverse values. It uses the data from the cache if the inverse of a value has already been calculated to save time.

cacheSolve <- function(x, ...) {
  #gets the inverse value if stored and outputs it
  slv <- x$getinverse()
  if(!is.null(slv)) {
    message("getting cached data")
    return(slv)
  }
  
  #calculates inverse if not already stored
  data <- x$get()
  slv <- solve(data, ...)
  x$setinverse(slv)
  slv
  
}
