## Put comments here that give an overall description of what your
## functions do
##  The two function in this assignment collectively allow calculating the inverse
## of a square matrix and caching the result for future retrieval
## There is a test with results at the botton of this file.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize to null
    mi  <- NULL
    
    ## set the value of the matrix
    set  <- function(y){
        x <<- y
        mi <<- NULL 
    }

    ##get the value of the matrix
    get  <- function() x
        
    ## function to set the value of the matrix inverse
    setinverse  <- function(inverse) mi  <<- inverse
    
    ## function to get the value of the matrix inverse
    getinverse  <- function() mi
    
    ## list of functions
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) 

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## get the inverse of matrix from getinverse function in makeCacheMatrix
    mi  <- x$getinverse()
    
    ## if inverse available then return it from cache
    if (!is.null(mi)){
        message("getting cached data")
        return(mi)
    }
    
    ## otherwise, get matrix and 
    data  <- x$get()
    ## calculate inverse using solve()
    mi  <- solve(data, ...)
    ## add matrix inverse to cache
    x$setinverse(mi)
    
    ## before returning
    mi
            
}

## To test the function above
## 1. define a matrix
# m <- matrix(c(3,2,6,2,7,0,2,4,8), 3) ## define a 3 by 3 (square) matrix
## 2. call the make cache matrix function
# A <- makeCacheMatrix(m)
## 3.call the cachesolve function
# b <- cacheSolve(A)
# b
## RESULT
#
#[,1]  [,2]  [,3]
#[1,]  0.56 -0.16 -0.06
#[2,]  0.08  0.12 -0.08
#[3,] -0.42  0.12  0.17
## call the cachesolve function again returns data from cache
## b <- cacheSolve(A)
##
# getting cached data
# b
# [,1]  [,2]  [,3]
# [1,]  0.56 -0.16 -0.06
# [2,]  0.08  0.12 -0.08
# [3,] -0.42  0.12  0.17




