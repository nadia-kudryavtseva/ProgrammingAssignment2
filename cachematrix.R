## The functions implement calculations of an inverse of a matrix with implemented
## functionality to retrieve a cashed solution (if the solution was already
## calculated). Here is an example how to use it:

##testing the functions:
##initialize the matrix
#   v <- makeCacheMatrix()
##set the matrix to a solvable example matrix
#   v$set(matrix(c(4,3,3,2), nrow=2, ncol=2))
##show that the matrix is set correctly
#  v$get()
##solve
#   cacheSolve(v)
#  cacheSolve(v)


## The function initializes a list of four functions - set, get, setsolve, getsolve

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The function calculates an inverse of a matrix x. In case the solution 
## already exists, the result is a cashed value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getsolve()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
}

