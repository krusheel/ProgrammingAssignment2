## These functions provide an ability to cache the inverse of matrix
## and return cached inverse if it is already computed

## Return a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL

        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x

        setSolve <- function(sol) s <<- sol
        getSolve <- function() s
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## returns an inverse of a matrix from cache if it is already computed
## else computes the inverse and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve
        if(!is.null(s)) {
                message("getting cached inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setSolve(s)
        s
}
