## Matrix inversion is usually a costly computation and there is  
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The functions here cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## The function, makeCacheMatrix creates a special "matrix", 
        ## which is really a list containing a function to: 
        ##  1.set the value of the matrix
        ##  2.get the value of the matrix
        ##  3.set the value of the inverse
        ##  4.get the value of the inverse
        
        i <- NULL
        set <- function(y) {
                x <<- matrix(y)
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- x
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## The following function calculates the inverse of the special "matrix" 
        ## created with the above function. 
        ## However, it first checks to see if the inverse has already been calculated. 
        ## If so, it gets the inverse from the cache and skips the computation. 
        ## Otherwise, it calculates the inverse of the matrix and sets the value of the 
        ## inverse in the cache via the setinverse function.
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
