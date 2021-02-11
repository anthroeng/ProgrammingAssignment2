## Assignment: Caching the Inverse of a Matrix
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
            setmatrix <- function(y){
                x <<- y
                m <<- NULL
                }
            getmatrix <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(setmatrix = setmatrix,
                 getmatrix = getmatrix,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
              message("getting cached matrix")
              return(m) ## Return a matrix that is the inverse of 'x'
            }
            matrix <- x$getmatrix()
            m <- solve(matrix, ...)
            x$setinverse(m) ## Cache result if not yet cached
            m
        
}



