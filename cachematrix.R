## Below are two functions that are used to create a special object that stores 
#a matrix and cache its inverse.

## The makeCacheMatrix creates a list containing 4 functions:
# 1. set and 2. get the value of the matrix, 3.set and 4.get the inverse of the
#matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## The cacheSolve function looks first in the cache if  the inverse of the 
#matrix has already been calculated. In this case it return the inverse from the
# cache. If not, the inverse must be calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}


# test our functions
l<-c(-1,3,-3,0,-6,5,-5,-3,1)
myMatrix<- matrix(l,nrow=,ncol=3)

myMatrix.cache <- makeCacheMatrix(myMatrix)
myMatrix.cache$get()

cacheSolve(myMatrix.cache)
cacheSolve(myMatrix.cache)

