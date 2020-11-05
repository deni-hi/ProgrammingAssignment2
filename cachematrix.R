makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverted <- function(inv) m <<- inv
    getinverted <- function() m
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}

cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" returned by
    ## makeCacheMatrix above. If the inverse has already been calculated 
    ## (and the matrix has not changed), then the cachesolve should retrieve 
    ## the inverse from the cache.
    m <- x$getinverted()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}
