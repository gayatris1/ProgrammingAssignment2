## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

 ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ## this list is used as the input to cacheSolve()

inv <- NULL
  set <- function(y) {
  
  # use `<<-` to assign a value to an object in an environment 
  # different from the current environment
                
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve is a function which computes the inverse of the special "matrix" 
## return: inverse of the original matrix input to makeCacheMatrix()
## and the matrix has not changed, then the cachesolve should retrieve the 
## inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
       
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Get cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
