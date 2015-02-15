## The following functions create and solve a special type of matrix which can
## cache its inverse. This is valuable when using a very large matrix, which 
## takes a long time to solve.
 
## This function creates a "matrix" that can cache its inverse.
## Its input parameter is a single matrix.
## It returns a list consisting of four functions that set and get the matrix,
## and which set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a "cacheMatrix", which is created by the
## makeCacheMatrix function.
## Its input parameter is a single cacheMatrix.
## It returns the inverse of the cacheMatrix. If the inverse has already been 
## calculated, it returns the cached value instead of calculating the inverse 
## again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
