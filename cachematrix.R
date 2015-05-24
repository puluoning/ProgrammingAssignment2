## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Cache the inverse of a matrix. Steps: 1. Set the value of the matrix; 2. 
        ## get the value of the matrix; 3. set the value of the inverse of the matrix; 
        ## 4. get the value of the inverse of the matrix
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Calculate the inverse of the special "matrix" makeCacheMatrix. 
        ## First checks to see if the Matrix inverse has already been calculated. 
        ## If so, it gets the matrix inverse from the cache and skips the computation. 
        ## Otherwise, it calculates the matrix inverse of the data and sets the value
        ## of the inverse of the matrix in the cache via the setInverse function
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
