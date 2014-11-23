## Below Function caches a matrix and returns its results


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL ##Sets the value of matrix inverse to NULL by default
  y <- NULL ##Sets to NULL by default
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y ## caches the input value
    mi <<- NULL ## sets the inverse to NULL
  }
  getmatrix <- function() x
  setmatrixinverse <- function(inverse) mi <<- inverse
  getmatrixinverse <- function() mi
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}


## function computes the inverse of the special "matrix" returned by makeCacheMatrix function,
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinverse() # get cached value
  if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    ## otherwise
    y <- x$getmatrix() ## get matrix
    x$setmatrix(y) ## set matrix
    m <- solve(y, ...) ## compute inverse ,R is awesome!!
    x$setmatrixinverse(m) ## set the inverse
    m ## return the inverse
}

## To test it out
## mat <- matrix(data = c(1,2,3,4) nrow = 2, ncol = 2)
## mat1 <- makeCacheMatrix(mat)
## cacheSolve(mat1)
## Ouput
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(mat1)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## mat <- matrix(data = c(1,2,3,5), nrow = 2, ncol = 2)
## mat2 <- makeCacheMatrix(mat)
## cacheSolve(mat2)
##    [,1] [,2]
## [1,]   -5    3
## [2,]    2   -1
