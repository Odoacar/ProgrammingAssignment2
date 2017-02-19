## Programming Assignment 2
## The assignment consists of writing a pair of functions 
## that calculate and cache the inverse of a matrix.
## Since matrix inversion is usually a costly computation,
## it is beneficial to cache the inverse of a matrix and call the cached inverse 
## if it has been computed already, instead of computing it repeatedly. 

## The first function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse. 
## It returns a list containing the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## The function initially sets the inverse to NULL
     inv <- NULL
     ## then it sets the matrix and the inverse to NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     ## it gets the matrix 
     get <- function() x
     ## sets the inverse
     setinverse <- function(inverse) inv <<- inverse
     ## get the inverse
     getinverse <- function() inv
     ## put it all in a list
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The "cacheSolve" function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache. 
## In this case a message is printed. 

## Example for how it works:
## Create a matrix a:
## a <- matrix(1:4, nrow=2, ncol=2)
## write it into the CacheMatrix with makeCacheMatrix:
## CachedMatrix <- makeCacheMatrix(a)
## Calculate the inverse with cacheSolve:
## cacheSolve(CachedMatrix)
## when calling cacheSolve(CachedMatrix) again, the cached inverse will be output
## together with the message "getting cached data".

cacheSolve <- function(x, ...) {
     ## This function returns a matrix that is the inverse of 'x'
     ## it gets the inverse and checks if it has already been computed,
     ## i.e. if inv is still NULL or contains the (already) inverted matrix
     inv <- x$getinverse()
     if(!is.null(inv)) {
          ## if inv has been computed already, the function returns the cached result
          message("getting cached data")
          return(inv)
     }
     ## if inv hasn't been computed yet, cacheSolve gets the matrix 'x' and
     ## computes its inverse with solve()
     data <- x$get()
     inv <- solve(data, ...)
     ## then the inverse is cached and output
     x$setinverse(inv)
     inv
}
