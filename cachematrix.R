## This file has two functions makeCacheMatrix and cachesolve
## makeCacheMatrix takes a regular R matrix (assumed invertable)
## cachsolve ue the object created by makeCacheMatrix
## Sample usage:
## x <- matrix(rnorm(100),10,10)  !creates a matrix
## x_new <-makeCacheMatrix(x)
## cachesolve(x_new) !returns the inverse
## cachesolve(x_new) !returns the cached inverse

## makeCacheMatrix buils a special "matrix" object
## The object has 4 functions 
## set loads the matrix
## get returns the value of the matrix
## setinverse  loads  the inverse of the matrix using solve()
## getinverse returns the inverse of the matrix

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
## cacheSolve returns the inverse of a matrix
## if the inverse has already been computed it returns a cached value


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
