## Creates cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y) {
                x <<- y
                inversed <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inversed <<- i
        getInverse <- function() inversed
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x' 
## using a cache if the 'x' does not change
cacheSolve <- function(x, ...) {
        
        inversed <- x$getInverse()
        if(!is.null(inversed)) {
                message("getting cached data")
                return(inversed)
        }
        
        data <- x$get()
        inversed <- solve(data, ...)
        x$setInverse(inversed)
        return (inversed)
}

