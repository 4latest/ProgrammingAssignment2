## Put comments here that give an overall description of what your
## functions do

## Function 'makeCacheMatrix' to create matrix object in cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
    set <- function(y) { ## Function to set value of matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x ## Function to get value of matrix
    
    setmatrix <- function(solve) m <<- solve  ## Function to set value of inverse
    getmatrix <- function() m                 ## Function to get value of inverse
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Function 'cacheSolve' to return a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()  ##Gets matrix
    if(!is.null(m)) {   ## Checks if inverse has already been computed
      message("getting inversed cached data") ## If so, it gets inverse from cache 
      return(m)         ## and skips computation.
    }
    data <- x$get()
    m <- solve(data, ...) ## Else, it calculates inverse of the data and set value into cache via setmatrix function.
    x$setmatrix(m)
    m
}
