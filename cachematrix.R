## The functions makeCacheMatrix and cacheSolve work together
## to help make the process of retrieving matrix inverses
## faster. makeCacheMatrix creates a list of four functions, 
## a get and set funtion for the matrix, and a get and set
## function for the inverse of that matrix. cacheSolve then uses
## this list of functions as its argument, and attempts to simply
## read off the inverse matrix from cache. If it isn't there, then 
## the matrix inverse is solved. However from now on, whenever
## that same inverse is needed again, it can simply be read off
## of cache, so any inverse only gets solved one time.

## makeCacheMatrix - takes a matrix, x, as its argument
##     This function creates a list of functions:
##     a get and set function for the matrix and
##     get and set functions for its inverse. The
##     value of inv, the inverse of x, is saved to
##     cache by the setinverse function.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
            inv <<- inverse
        }
        getinverse <- function() inv
        list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
        }



## cacheSolve - takes as its argument a list of functions
##     created by the makeCacheMatrix function. This function
##     attempts to read the matrix inverse value, inv, from 
##     cache using the x$getinverse function from the list, x. If
##     the value of inv is not null, then inv is returned. If inv
##     is null, then the inverse is solved, the x$setinverse 
##     function saves this value to cache, and inv is returned.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("Getting cached data.")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
	inv
}
