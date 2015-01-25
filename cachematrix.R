## These functions are to speed up the process of inversing a matrix. This is
## done by creating a special object to "cache" the matrix. The function makeCacheMatrix
## must be run first for this assignment to create that special object. If the 
## original matrix is changed, the makeCacheMatrix *must* be run again to 
## accurately contain the inverse.

## This function assumes the given matrix will always be inversible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## matrix is cleared to create new special "matrix" object and setting matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## set the inverse of the matrix by rebinding
  ## setmatrix in the parent of the current environment.
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  
  ## create list for all functions
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        ## gets the matrix from cache if it exists
        m <- x$getmatrix()
      
        ## check if matrix exists in special object cache
        if(!is.null(m)) {
          message("getting cached data")
          ## Return a matrix that is the inverse of 'x' that was stored in
          ## "special matrix" object when it was created
          return(m)
        }
        ## the inverse matrix was not previously stored so we need to set it
        data <- x$get()
        ## compute the inverse of the special matrix with given data
        m <- solve(data, ...)
        ## set new m
        x$setmatrix(m)
        ## output the new m value
        m
}
