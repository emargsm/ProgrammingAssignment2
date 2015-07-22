## This is a pair of functions to make matrix inversion more efficient.
##
## makeCacheMatrix will store a matrix with its given inverse. It will also
## retrieve any stored inverse.
##
## cacheSolve will first check whether an inverse is already stored with a 
## matrix x. If it is, cacheSolve will simply return a message that it's using 
## the stored inverse, and will return the stored inverse.
## If there is no stored inverse, cacheSolve will perform the matrix inversion
## (i.e. will solve for the inverse) and will store the inverse with the 
## associated matrix.
##
## This solution is based very heavily on the instructions
## https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions
## and
## the clarifying instructions from Daniele Pigni
## https://github.com/DanieleP/PA2-clarifying_instructions

## The function makeCacheMatrix has the following four sub-functions:
## 1. set(). Store a matrix.
## 2. get(). Retrieve a matrix.
## 3. setinverse(). Store some matrix as the inverse of the calling matrix.
## 4. getinverse(). Retrieve the stored inverse, if any.
## NOTE: This function does not actually calculate the inverse, nor does it
##       perform any sort of check on the values. It is a simple setter/getter
##       that will only do as it's told.
## NOTE: We are assuming that we're getting a square, invertible matrix.
##       This function does not check the dimensions or invertability.

makeCacheMatrix <- function(x = matrix()) {
  # Start with no inverse
  inverse <- NULL
  
  # If a matrix 'y' is passed with set(), set 
  # the matrix 'x' to 'y'.
  # Simultaneously, set the current 'inverse'
  # back to NULL, since a new matrix can't
  # have an inverse yet.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # When get() is called, simply return the
  # current 'x'.
  get <- function() x
  
  # If a value 'solve' is passed with
  # setinverse(), set the value of variable
  # 'inverse' to 'solve'.
  setinverse <- function(solve) inverse <<- solve
  
  # When getinverse() is called, return the
  # current 'inverse'.
  getinverse <- function() inverse
  
  # Make a list of the sub-functions, so that 
  # they can be called with the $ format.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function, cacheSolve, checks whether the given matrix has a stored
## inverse. If it does, cacheSolve does no computation, and simply returns
## the stored inverse.
## However, if there is no cached inverse -- i.e. inverse is NULL -- cacheSolve
## uses R's 'solve' function to determine the inverse and then store it
## with the matrix.
## NOTE: We are assuming that we receive a square, invertible matrix. This
##       function does not perform any checks on this.
## NOTE: Similarly, the instructions state "If the inverse has already been 
##       calculated (and the matrix has not changed), then...." We are relying
##       on the set() function within makeCacheMatrix to reset the inverse to
##       NULL as soon as the matrix is changed. We do not perform an explicit
##       check to make sure the matrix is the same.

cacheSolve <- function(x, ...) {
    # First, get any stored 'inverse' from 'x'.
    inverse <- x$getinverse()
    
    # If it already exists, tell the user it's
    # cached. Return the current 'inverse'.
    # Don't do any calculation.
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    # Because the 'if' above has a return statement,
    # we can treat everything past this point as being
    # within one big 'else'.
    
    # Since we know there's no current value in 'inverse',
    # get the contents of 'x' so that we can prepare to find
    # the actual inverse matrix.
    data <- x$get()
    
    # Use R's solve() function to find the inverse matrix.
    # a %*% x = b
    # In this case, we can just pass 'data' as 'a'. 'b'
    # is assumed to be the identity matrix, making 'x'
    # the inverse matrix of a.
    inverse <- solve(data, ...)
    
    # Now that we have the inverse, store it inside 'x'.
    x$setinverse(inverse)
    
    # Return the inverse matrix as well.
    inverse
}
