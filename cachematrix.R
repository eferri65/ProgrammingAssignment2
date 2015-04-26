## This function makeCacheMatrix creates a special "vector", which is a list 
## containing 4 functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        ## the cache s initialized her
        m <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse matrix in the cache m
        setinversematrix <- function(inversematrix) m <<- inversematrix
        ## get the value of the inverse matrix
        getinversematrix <- function() m
        ## creates a special "matrix"
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix x
        m <- x$getinversematrix()
        ## if the inverse of the matrix x is already in the cache, there is nothing to compute
        ## and return it  
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if  the inverse of the matrix is not computed yet (i.e. m is NULL) then get the matrix
        matrix <- x$get()
        ## compute the inverse of the matrix
        ## use the function solve to compute the inverse
        m <- solve(matrix, ...)
        ## save the inverse of the matrix in the cache
        x$setinversematrix(m)
        ## return the inverse the matrix
        m
}
