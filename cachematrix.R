## These two functions will take a matrix, calculate its inverse and cache the result
## in case it is again needed; to avoid making the costly calculation more than once.


## makeCacheMatrix takes a matrix and creates a matrix object that stores its inverse.
## It returns a list of four functions: set value, get value, set value of inverse matrix 
## and get value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      # initialize inverse matrix
      invM <- NULL
      
      # set the values for original matrix and inverse matrix OUTSIDE the execution
      # environment of set()
      set <- function(y) {
            x <<- y
            invM <<- NULL
      }
      
      # get and return the original matrix
      get <- function() {
            return(x)
      }
      
      # set the result for the inverse of matrix using ?solve
      setinverse <- function(solve) {
            invM <<- solve
      }
      
      # get inverse of matrix
      getinverse <- function () {
            return (invM)
      }
      
      # implicitly returns a list of functions. They can be called using $ operator, as 
      # can be seen in the next function.
      list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Checks whether the inverse matrix has been already calculated, if it has and the
## matrix has not changed, retrieves cached data. If not (if inverse matrix is not found, 
## so NULL) then moves on to calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
      
      # get inverse matrix using the function getinverse from the returned output of the
      # function makeCacheMatrix.
      invM <- x$getinverse()
      
            # If we do have the inverse matrix stored, return cached inverse matrix.
            if(!is.null(invM)) {
                  message ("getting cached data")
                  return (invM)
            }
      
      # If we do NOT have the inverse matrix stored, save the original matrix using get 
      data <- x$get()
      #... pass it to the ?solve function to get the inverse matrix
      invM <- solve(data, ...)
      # set the inverse matrix for storing in case we need it later...
      x$setinverse(invM)
      
      # return stored inverse matrix
      return(invM)

}
