## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix - creates a special matrix object. Returns a list of set/get accessor
## functions for the matrix and its inverse
## 
## cacheSolve - returns the inverse of an invertable matrix object created by makeCacheMatrix(), fist checking to see if
## a cached result is available before returning the calculated inverse


## makeCacheMatrix - creates a matrix object and returns list of accessor functions
makeCacheMatrix <- function(x = matrix()) {
  
  set <- function(y) {
    m_inv <<- NULL
    x <<- y
  }
  
  get <- function() x
  
  set_inv <- function(matrix) m_inv <<- matrix
  
  get_inv <- function() m_inv
  
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)

}


## cacheSolve - Returns the inverse of an invertible matrix object defined by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  
  if(!is.null(m)) {
    message("using cached inverse")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$set_inv(m)
  
  m
}
