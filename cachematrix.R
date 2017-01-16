## This files contains two functions makeCacheMatrix and cacheSolve
###
## makeCacheMatrix creates a special "matrix" object
##     that can cache its inverse
## cacheSolve computes the inverse of the special 
##     "matrix"returned by makeCacheMatrix
##     if the inverse has already been calculated,
##     it retrieve the inverse from the cache

## makeCacheMatrix accept a matrix as argument and
## returns a list of four function:
##      set(matrix())   :   to set the value of the matrix
##      get             :   to get the value of the matrix
##      setsolve        :   to set the value of the inverse matrix
##      getsolve        :   to get the value of the inverse matrix      

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    list( set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## cacheSolve co,pute the inverse of the special matrix created by makeCacheMatrix
##      if the inverse has already been computed it returns the value from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getsolve()
    if (!is.null(inverse)) {
        message("Getting cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setsolve(inverse)
    inverse
}
