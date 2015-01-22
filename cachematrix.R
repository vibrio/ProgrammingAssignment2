## Caching the Inverse of a Matrix

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        rm <- NULL
        set <- function(y) {
                x <<- y
                rm <<- NULL
        }
        get <- function() x
        setrmatrix <- function(rmatrix) rm <<- rmatrix
        getrmatrix <- function() rm
        list(set = set, get = get,
             setrmatrix = setrmatrix,
             getrmatrix = getrmatrix)
        
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        rm <- x$getrmatrix()
        if(!is.null(rm)) {
                message("getting cached data")
                return(rm)
        }
        data <- x$get()
        rm <- solve(data, ...)
        x$setrmatrix(rm)
        rm
}
