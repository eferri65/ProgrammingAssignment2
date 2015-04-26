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
        ## return the inverse of the matrix
        m
}

## my tests
## setwd("ProgrammingAssignment2")
## source("cachematrix.R")

## -- test 1 : cacheSolve does NOT compute the inverse because the inverse is in the cache --
## x<-matrix(c(4,2,7,6),nrow=2,ncol=2)
## inversex<-solve(x)
## specialMatrix<-makeCacheMatrix()
## specialMatrix$set(x)
## specialMatrix$get()
## -- Result: you should get 
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## --SETTING THE CACHE
## specialMatrix$setinverse(inversex)
## specialMatrix$getinverse()
## -- Result: you should get
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## --cachesolve should "see" the cache already contains the inverse of the matrix x
## cacheSolve(specialMatrix)
## -- Result: you should get
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## -- test 2: cacheSolve DOES compute the inverse because the cache is empty --
## x<-matrix(c(3,3.5,3.2,3.6),nrow=2,ncol=2)
## specialMatrix<-makeCacheMatrix()
## specialMatrix$set(x)
## specialMatrix$get()
## specialMatrix$getinverse()
## -- Result: the cache is empty and you should get
## NULL
## cacheSolve(specialMatrix)
## -- Result: you should get
## [,1] [,2]
## [1,] -9.00  8.0
## [2,]  8.75 -7.5