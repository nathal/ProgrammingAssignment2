## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##this is the set matrix function
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##this is the get matrix function
        get <- function() x
        #this is the set inverse function
        setinv <- function(inverse) m <<- inverse
        #this is the get inverse function
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculates, the cachesolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ##x is the output from makeCacheMatrix
        ##get inverse of the matrix 
        m <- x$getinv()
        ##if the inverse has already been calculated, 
        ##then the cachesolve retrieves the inverse from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##if the inverse has not been calculated yet, get matrix
        data <- x$get()
        ##and calculate inverse of matrix
        m <- solve(data, ...)
        #and set the inverse in the cache with the setinv function
        x$setinv(m)
        m
}
