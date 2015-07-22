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
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
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
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
