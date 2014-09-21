## This R script contains two functions. The first function returns a list of functions.
## The returned list of functions can be interpreted as a "special" matrix that can cache
## its own inverse. Once cached, the inverse does not need to be computed again.
## The second function returns the inverse of the cached matrix. If the inverse was already 
## computed, it will be extracted from the cache, otherwise the inverse will be computed
## and then cached using the "special" matrix.


## makeCacheMatrix - This function takes a matrix as an input, and returns a list of functions.
## The returned list can set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set -  Caches the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get -  Returns the cached value of the matrix
        get <- function() x
        # setinverse - Caches the inverse
        setinverse <- function(inverse){
                inv <<- inverse
        }
        # getinverse - Returns the cached inverse
        getinverse <- function() inv
        list(set = set , get = get , setinverse = setinverse , getinverse = getinverse)
}

## cacheSolve - This function returns the inverse of a matrix x. The input of this functin is
## the list of functions computed by makeCacheMatrix. If the inverse exists in the cache,
## then a message will be displayed, and the inverse returned. Otherwise, the function
## will compute the inverse, cache it for future use, and return it.
## Note that the solve() function was used for this exercise, which only takes square matrices.
## A more general implementation would use the pinv() or ginv() functions, from the PRACMA
## package, and the MASS package, respectively.

cacheSolve <- function(x, ...) {
        # Getting the inverse from the cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Cached data found - Inverse already computed")
                return(inv)
        }
        # Getting the cached value of the matrix
        data <- x$get()
        # Computing the inverse, if it wasn't found in the cache
        inv <- solve(data, ...)
        # Caching the inverse
        x$setinverse(inv)
        inv
}
