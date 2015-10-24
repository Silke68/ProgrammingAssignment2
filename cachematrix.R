## These two functions are able to cache the inverse of a matrix. So that when
## we need it again, it can be looked up in the cache rather than recomputed. 


# This function creates a special "matrix" object that can cache its inverse.
# It returns a list with functions to set and get the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
} 



# Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}