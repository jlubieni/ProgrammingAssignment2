## The two functions below cache the inverse of a matrix


## the makeCacheMatrix is a function that creates a special
## matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y){
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) invMat <<- matrix
        getinverse <- function() invMat
        list(get = get, set = set,
             getinverse = getinverse,
             setinverse = setinverse)

}


## This function computes the inverse of the "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        invMat <- x$getinverse()
        if(!is.null(invMat)){
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setinverse(invMat)
        invMat
}
