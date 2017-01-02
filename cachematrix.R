## Put comments here that give an overall description of what your
## functions do

## ## This function computes the inverse of the matrix returned by makeCacheMatrix (see above).
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache. If so, the text
## "Get cache data for Elsa" will appear

Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_els <- NULL
  set <- function(y){
    x <<-y
    inv_els <<- NULL
  }
  get <- function() x
  setInverse<- function(Inverse) inv_els <<-Inverse
  getInverse <- function() inv_els
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_els <- x$getInverse()
    if(!is.null(inv_els)) {
      message("Get cache data for Els")
      return(inv_els)
    } 
    ## this checks if the inverse of matrix 'x' is already available in cache
    else{
      data <- x$get()
      inv_els <- solve(data)
      x$setInverse(inv_els)
      inv_els
    }
    ## Return a matrix that is the inverse of 'x'
}

