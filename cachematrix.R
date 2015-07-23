## These two functions calculate and cache the inverse of a matrix.

## This function does the caching part:

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


## This function calculates the inverse of the matrix. However, it first checks
## for cached data and retrieves it if found.

cacheSolve <- function(x, ...) {
        ## Check for cached data
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
