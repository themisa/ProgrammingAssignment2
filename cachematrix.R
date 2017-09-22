## Put comments here that give an overall description of what your
## functions do
# This assignment was made clearer by the Demystifying makeVector() post by a course tutor!
# This file contains a pair of functions that cache the inverse of a matrix
# We assume that the inverse exists!

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #  object to be used as inverse matrix initialized to NULL
  set <- function(y) {  # this is the "set" function to set the values of var x, inv in the parent environment
    x <<- y             # assigns the value y to the x object in parent env.
    inv <<- NULL        # assigns value NULL to inv object. Clears any value on inv that had been cached  
  }
  get <- function() x  # this function just gets x from parent environment
  setinv <- function(inverse) inv <<- inverse # function that sets the inverse matrix value from parent env.
  getinv <- function() inv     # function that gets inverse from parent.
  list(set = set, get = get,   # all the defined functions become elements of a list that is returned to parent env.
       setinv = setinv,        # so we can access them using the a$b construct.
       getinv = getinv)
}




## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()      # gets the value of the inverse from parent env.
  if(!is.null(inv)){     # checks to see if it is NULL. If not, it gets cached value
    message("getting cached inverse matrix")
    return(inv)
  }
  original <-x$get()     # if the inverse is not cached we need to compute! First get original matrix
  inv <- solve(original) # the solve function calculated inverse of original matrix
  x$setinv(inv)
  inv                     # function returns inverse
  


}
