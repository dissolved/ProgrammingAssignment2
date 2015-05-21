## These two functions work in tandam to create a special cached version of a
## matrix, so that solving for the inverse of the matrix (which is potentially
## time consuming), can be cached for future access without computing again.
## Example usage is as follows:
##      > cm <- makeCacheMatrix(matrix(c(2,2,3,2), nrow=2, ncol=2))
##      > cacheSolve(cm)
##           [,1] [,2]
##      [1,]   -1  1.5
##      [2,]    1 -1.0


## makeCacheMatrix creates a special "matrix", which is really a list
## containing these functions:
##      set()        - set the value of the matrix
##      get()        - return the value of the matrix
##      setinverse() - set the value of the inverse matrix
##      getinverse() - return the value of the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                inv
        }
        else{
                inv <- solve(x$get())
                x$setinverse(inv)
                inv
        }
}
