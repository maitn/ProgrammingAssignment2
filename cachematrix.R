## makeCacheMatrix and cacheSolve reduce computing time by storing inverse of 
## matrix in cache.

## V2 - 08/21/2015. Mai Nguyen. Created functions using makeVector and
## cachemean example

## makeCacheMatrix - stores function calls for matrix x.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setInvert <- function(invertM) m <<- invertM
        getInvert <- function() m
        list(set = set, 
		 get = get,
             setInvert = setInvert,
             getInvert = getInvert)
}

## cacheSolve - if inverse matrix x is stored in cache returns cache value of 
## inverse otherwise calls solve function to create inverse and store in cache 
## for later retrieval 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
        m <- x$getInvert()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setInvert(m)
        m
}