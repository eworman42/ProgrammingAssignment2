## makeCacheMatrix will overall produce a matrix object, and cache
## its inverse.
## 
## It contains functions:
##      1. set: Set the origin matrix.
##      2. get: Gets the origin matrix.
##      3. setInv: Sets the inverse of the origin matrix.
##      4. getInv: Gets the inverse of the origin matrix.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
         x <<- y
         inv <<- NULL
    }
     get <- function() x
     setInv <- function(inverse) inv <<- solve(x)
     getInv <- function() inv
     list(set = set, get = get, setInv = setInv, getInv = getInv)
    

}


## cacheSolve() returns the inverse of the makeCacheMatrix() matrix cache.
## Displays a message prompting you to be patient while it
## returns the inverse from cache.

cacheSolve <- function(x, ...) {
       inv <- x$getInv()
       if (!is.null(inv)) {
           message("Hold your horses!")
           return(inv)
       }
       invData<-x$get()
       inv<-solve(invData, ...)
       x$setInv(inv)
       inv
}