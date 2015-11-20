## This functions caches the inverse of a matrix
## It has two fuctions one cache the Matrix (makeCacheMatrix) and second inverse 
## the matrix (cacheSolve)it first checks to see if the Inverse has already been 
##calculated.If so, it gets the Cached data from the cache and skips the 
##computation. Otherwise, it calculates the Inverse of the data and sets 
##the value of the matrix in the cache via the setsolve function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function gives the inverse of a Special matrix returned from the 
##makeCacheMatrix above

cacheSolve <- function(x, ...) {
## This code give the inverse of the matrix        
        m <- x$getsolve()
##If the data is cached in makeCacheMatrix for the matrix then it gets the data         
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
##If its a new matrix then it gets and compute the data and put it in m variable
##so that can be used later when needed        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
    
}



