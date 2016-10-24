## This file has 2 functions. One called "makeCacheMatrix". The second one called "cacheSolve".
## At the surface of it, it seems like a matrix x can be passed to both these functions.
## However, the implementation of this should be done in 2 steps. First you need to cache the inverse of the matrix object.
## You do this by having mat1 <- makeCacheMatrix(B), where B is the input matrix.
## Now "mat1" is the matrix that has all the cached properties. Now you pass mat1 to the second function in this file.
## You do this by calling cacheSolve(mat1).

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## It creates the matrix object which needs to be passed to the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
        ## initiatilize the inverse matrix to NULL
        inv <- NULL
        set <- function(x) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve  ## calculates the inverse using "solve" function
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        print(x)
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if the inverse is not in cache, it will calculate it here.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
