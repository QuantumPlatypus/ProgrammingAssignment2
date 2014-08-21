## These are functions intended to introduce the concept of lexical scoping
## Inversion of a matrix is computationally expensive.  If a matrix does not change
## regularly and if the inverse is needed often, then it is worth caching the inverse
## to save the cost of recomputing.  These functions create a matrix object with a
## cache for the inverse and calculate or recover the inverse as needed

## This function creates a matrix object that includes a cache to store the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     
     #Initialize the cache
     
     m <- NULL
     
     #Initialize the new object
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     
     # Define the operations on the object and return them in a list
     # This allows any of the functions to be called using x$function()
     
     get <- function() x
     setinv <- function(solution) m <<- solution
     getinv <- function() m
     
     #this returns the list - in R the last call is returned
     
     list(set = set, get = get, setinv = setinv, getinv = getinv )
     
}


## This function uses the special matrix class above and returns the inverse.  If there
## is no cached inverse then the function calculates the inverse; otherwise it returns the 
## cached value

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     
     if(!is.null(m)) {
          # If there is a cache, return the cache
          message("Retrieving cached data")
          return(m)
          
     } else {
          # Solve the matrix if needed
          data <- x$get()
          m <- solve(data, ...)
          x$setinv(m)
          return(m)
     }
}
