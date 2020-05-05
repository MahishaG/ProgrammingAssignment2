## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             ##set inv to null
        set <- function(y)
              {
                x <<- y          ##assign the value ofmatrix in the parent environment
                inv <<- NULL
              }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse     ##assign the value of inverse of matrix in the parent environment
        getinverse <- function() inv                        ##get the inverse of matrix
        list(set=set,get=get,setinverse = setinverse, getinverse = getinverse)
        ## the above line returns the list of functions to set and get the matrix and its inverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()     ##gets the inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)          ##return the inverse if its already created 
        }
        data <- x$get()           ##the matrix is obtained
        inv <- solve(data, ...)    
        x$setinverse(inv)          ##inverse of the matrix is calculated and
        inv                        ##returned
}
