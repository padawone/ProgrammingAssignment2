## Assignment: Caching the Inverse of a Matrix
## This function gets a matrix and cache its inverse on memory

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


## Function that solve inverse matrix it check if previous inverse exists
## also check if current matrix is identical to inverse of inverse (originalMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        originalMatrix <- m$getinverse()
        if(!is.null(m) && identical(x,originalMatrix)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
