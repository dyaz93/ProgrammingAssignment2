## These functions takes a matrix, creates a special "matrix" object,
## calculates its inverse and puts it in a cache

## Creates a "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinv <- function(inv) v <<- inv
    getinv <- function() v
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## "Solves" the matrix, or retrieves the inverse if it's already in the cache

cacheSolve <- function(x, ...) {
    v <- x$getinv()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)    
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinv(v)
    v       ## Return a matrix that is the inverse of 'x'
}
