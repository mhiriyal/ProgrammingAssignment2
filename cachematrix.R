##          ** Caching the Inverse of a Matrix **



## makeCacheMatrix: This function creates a special "matrix" object that 
##          can cache its inverse.
##


makeCacheMatrix <- function(x = matrix()) 
{
      invrs <- NULL
      setmatx <- function(y) {
            x <<- y
            invrs <<- NULL
      }
      getmatx <- function() x
      setinvrs <- function(inverse) invrs<<- inverse
      getinvrs <- function() invrs
      list(setmatx = setmatx, getmatx = getmatx, setinvrs = setinvrs,
           getinvrs = getinvrs)

}


## cacheSolve: This function computes the inverse of the special "matrix" 
##          returned by makeCacheMatrix above. If the inverse has already 
##          been calculated (and the matrix has not changed), then the 
##          cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
      invrs <- x$getinvrs()
      if(!is.null(invrs)) {
            message("getting cached Inverse Matrix")
            return(invrs)
      }
      matrx <- x$getmatx()
      invrs <- solve(matrx, ...)
      x$setinvrs(invrs)
       retrun(invrs)
}
