## makCacheMatrix.R creates a special vector of functions that stores the inverse of 
## the input matrix in the cache. cacheSolve.R Supplies the inverse of the matrix, 
## either by retrieving the matrix from the cache or calculating it. 

# Run 1st. makeCacheMatrix takes an invertible matrix as input, and creates a list out of four functions. 
# Sets the value of the matrix and the empty inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        # set the value of the matrix
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the inverse
        setinverse <- function(inverse) m <<- inverse
        
        # get the value of the inverse
        getinverse <- function() m
        
        # define output as a list of these functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Run 2nd. Takes the list made by makeCacheMatrix as input. Checks if inverse of 
# matrix has already been calculated - if it has, it is retreived from cache, if
# not it is calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the value of m at this time
        m <- x$getinverse()
        
        # if m is the same inverse get the cached inverse
        if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
        }
        
        # assign the original matrix x to data
        data <- x$get()
        
        # calculate inverse of data
        m <- solve(data, ...)
        
        # set the input to be m - the inverse
        x$setinverse(m)
        m
}
