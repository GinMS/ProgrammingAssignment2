## Caching the Inverse of a matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions provide the feature of caching the inverse of the matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m_cache <- NULL
  set_value <- function(y){
                x <<- y
                m_cache <<- NULL
            }
  getmatrix <- function() x
  setmatrix <- function(m_inverse) m_cache <<- m_inverse
  getinverse <- function() m_cache
  list (set_value = set_value, getmatrix = getmatrix, setmatrix = setmatrix, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  m_cache <- x$getinverse()

  if (!is.null(m_cache))
  {
    message("getting cached data")
    return (m_cache)
  }
  
  m_data <- x$getmatrix()
  m_cache <- solve(m_data, ...)
  x$setmatrix(m_cache)
  
  m_cache
  
}
## Return a matrix that is the inverse of 'x'
