############################################################################
## Functions developed part of R programming course held by Johns Hopkins ##
## Bloomberg School of Public Health at coursera.org.                     ##
## Helper functions implemented to potentially save time at the costly    ##
## operation of inverting a matrix.                                       ##
############################################################################

#' Holds a matrix together with its cached inverse.
#'
#' @param x A matrix initialized by matrix()
#' @return A list of functions moderating a matrix and its inverted equivalent
#' @examples
#' m <- makeCacheMatrix(matrix(1:4,2,2))
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#' Returns the inverse of a cached matrix.
#' 
#' @param x A cached matrix created with @makeCacheMatrix
#' @return The inverse of the cached matrix(x)
#' @examples
#' 
#' m<-matrix(trunc(rnorm(2048*2048)), 2048,2048)
#' m_cached <- makeCacheMatrix(m)
#' system.time(cacheSolve(m_cached))
#'   user  system elapsed 
#'   7.95    0.03    7.98
#'
#' system.time(cacheSolve(m_cached))
#' getting cached data
#'    user  system elapsed 
#'       0       0       0
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
