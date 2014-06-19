## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix <- function(x = matrix()) {

##}


## Write a short comment describing this function

##cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
##}



## The two functions below are defined to :
## a) take a matirx and cache its inverse; and
## b) can calculate the inverse for the matrix entered or use it from cache

## This function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      # set the initial value for inverse  = NULL
      inv <- NULL
      
      # set the values in matrix x to those taken from input argument 'y' given to function 'set()'
      set <- function(y) {
            # use of '<<-' to make 
            #the values for 'x' ('y' having local scope only) and 'inv' exist globally
            x <<- y
            inv <<- NULL
      }
      
      # get the values stored in matrix
      get <- function(){
            x
      }
      
      # function to set the inverse matrix values to 'inver' matrix passed to setinv() functoin
      setinv <- function(inver) {
            inv <<- inver
      }
      
      # function to get the inverse matrix values stored in 'inv'
      getinv <- function() {
            inv
      }
      
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}


## This function 'cacheSolve' computes the inverse of matrix, using the solve(x) function.
## solve(x) returns the inverse of matrix 'x'. Before calculating the inverse it will check if the
## inverse has already been calculated and also if the matrix has not changed. 


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      # gets the inverse from 'x' through getinv function
      m <- x$getinv()
      #checks if inverse already exists
      if(!is.null(m)) {
            message("getting cached data")
            # returns the already cached mean, if it exists
            return(m)
      }
      
      #get new matrix data using get function of x
      data <- x$get()
      # calculate the inverse of matrix
      m <- solve(data)
      # set the new value of inverse
      x$setinv(m,...)
      # return the new inverse
      m
}