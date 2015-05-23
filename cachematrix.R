## The first function, makeCacheMatrix creates a matrix, which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of the matrix
##  4. get the value of the inverse of the matrix

makeCacheMatrix <- function(inp_matrix = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        inp_matrix <<- y
        inv_matrix <<- NULL
    }
    get <- function() inp_matrix
    setmean <- function(solve) inv_matrix <<- solve
    getmean <- function() inv_matrix
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## cache solve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(inp_matrix, ...) {
    inv_matrix <- inp_matrix$getinverse()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- inp_matrix$get()
    inv_matrix <- solve(data, ...)
    inp_matrix$setinverse(inv_matrix)
    inv_matrix
}
