# There are a total of two functions which make use of R's lexical scooping rules.

# makeCacheMatrix initializes a default matrix (if not given as an argument to the function) and then sets the inverse to NULL. 
#It has functions to set the matrix, get the matrix, set the inverse and get the inverse defined in its own environment. 


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
	      x <<- y
    inv <<- NULL
      }
  get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve takes an object of type makeCacheMatrix.
#x$getmatrix() returns the original matrix either passed previously to makeCacheMatrix / or the $setmatrix function invoked on the list returned by the makeCacheMatrix.
# After finding the inverse, the setinv() function is called to set the inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv <- x$getInverse()
  if(!is.null(inv)){
	      message("getting cached data")
    return(inv)
      }
  data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
      inv      

}
