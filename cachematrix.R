## Matrix inversion is usually a costly computation and therefore it may be
## beneficial to cache the inverse of a matrix rather than computing it
## repeatedly. Below are a pair of functions that cache the inverse of a
## matrix.

## makeChacheMatrix creates a spcial "matrix" object, which is really a list
## containing a function to get/set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    
    # set the matrix
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set the inverse of the matrix in cache
    setinv <- function(matinv) minv <<- matinv
    
    # get the inverse of the matrix from cache
    getinv <- function() minv
    
    # list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets the
## inverse matrix in the vache via the setinv function
cacheSolve <- function(x, ...) {
    minv <- x$getinv()          ## get cached value of the inverse
    if (!is.null(minv)) {   
        message("getting cached data")
        return (minv)           ## return inverse and skip further processing
    }   
    data <- x$get()             ## get data using above function
    minv <- solve(data, ...)    ## calculate inverse of data
    x$setinv(minv)              ## set inverse in the cache
    minv                        ## Return the inverse
}
