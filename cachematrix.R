## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly
##
## We therefore have the following 2 functions:
##
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##     that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix` above. If the inverse has
##     already been calculated (and the matrix has not changed), then
##    `cacheSolve` retrieve the inverse from the cache.


## `makeCacheMatrix` creates a special list containing functions to
##
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the value of the inverse matrix
##   4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    # `inverse matrix` is initialized to NULL
    s <- NULL

    # Every time the matrix changes, the `inverse matrix` is reset to NULL
    # so it will be calculated again
    set <- function(y) {
            x <<- y
            s <<- NULL
    }

    # Return the `inverse matrix'
    # It may be NULL
    get <- function() s

    # Cache the `inverse matrix` in the parent environment
    setsolve <- function(solve) s <<- solve

    # Get the cached `inverse matrix` from the parent environment
    getsolve <- function() s

    # Return the list - really a function table
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Get the cached `inverse matrix`
    s <- x$getsolve()

    # If cached `inverse matrix` is not NULL,
    # then just return the cached `inverse matrix`
    if(!is.null(s)) {
            message("getting cached data")
            return(s)
    }

    # Here `inverse matrix` is NULL - either because the "cacheMatrix" object
    # has just been created or because the matrix value has been set again,
    # we must recompute the `inverse matrix` and cache it for later reuse

    # Get the original matrix value
    data <- x$get()

    # Compute the `inverse matrix` by calling solve
    # "..." are additional parameters that we will pass to the function "solve"
    s <- solve(data, ...)

    # Cache the `inverse matrix`
    x$setsolve(s)

    # Return the `inverse matrix`
    s
}
