## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inmat <- NULL
        set <- function(y) {
                x <<- y
                inmat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inmat <<- inverse
        getinverse <- function() inmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inmat <- x$getinverse()
        if(!is.null(inmat)) {
                message("getting cached data")
                return(inmat)
        }
        data <- x$get()
        inmat <- solve(data, ...)
        x$setinverse(inmat)
        inmat
}
