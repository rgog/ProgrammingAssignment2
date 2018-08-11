## Functions for Coursera R Programming week 3 assignment.
## These functions will be used to calculate inverse of a matrix and store it in cache
## so that if they are referred to again, cache value can be used instead of
## recalculating inverse.

## makeCacheMatrix creates a special matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  x_inv = NULL
  setMatrix <- function(y)
  {
    x<<-y
    x_inv<<-NULL
  }
  getMatrix <- function()
  {
    x
  }
  setXInv <- function(inverse)
  {
    x_inv <<- inverse
  }
  getXInv <- function()
  {
    x_inv
  }
  ## creating this list is required to refer to the functions.
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setXInv = setXInv, getXInv = getXInv)
}


## cacheSolve method is used to calculate inverse of the matrix created by makeCacheMatrix.
## If the inverse is already caluclated and the matrix has not changed, this method will
## get the value from the cache instead of calculating the inverse again.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getXInv()
  if(!is.null(inverse))
  {
    message("Getting Cache Inverse")
    return(inverse)
  }
  else
  {
    mat_data<-x$getMatrix()
    inverse<- solve(mat_data,...)
    x$setXInv(inverse)
    inverse
  }
}
