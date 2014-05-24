## functions will look for a cached inverse of a matrix. It one does not exist, it will calculate
## and cache the invesre matrix then return said matrix.  If it does exist it will return the inverse matrix.

## creates a special matrix that caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculates the inverse of the above function and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        cacheinverse <- function(x, ...) {
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                 }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
        }
                m
        
}
