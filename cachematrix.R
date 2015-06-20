## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions are used to catche the inverse of the matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
 

makeCacheMatrix <- function(x = matrix()) {             
        inv <- NULL     
        setmatrix <- function(y){
                x <- y
                inv <- NULL
        }
        getmatrix <- function()
                x
        setinverse <- function(inverse)
                inv <- inverse
        getinverse <- function()        
                inv
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv))
                return()
        data <- x$getmatrix()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


