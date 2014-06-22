## This function creates a special "matrix" object that can 
##cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##sets m to a null matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## sets the function to solve()
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ##checks to see if data is null, returns message if it is
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ##solves the inverse of matrix 'x' and returns result
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
