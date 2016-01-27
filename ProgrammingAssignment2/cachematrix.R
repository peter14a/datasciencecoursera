## The following two functions are used to cache the inverse of a matrix. 

## makeCacheMatrix:
## The makeCacheMatrix function creates a special 'matrix' object that can cache it's inverse.
        ## Use 'set' to set the value of the matrix
        ## Use 'get' to get the value of the vector
        ## Use 'setinv' to set the inverse of the matrix
        ## Use 'getinv' to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve Function: 
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
