## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	set <- function(y) {
        x <<- y
        inv <<- NULL
    }
	
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix=setmatrix, getmatrix=getmatrix, 
		 setinverse=setinverse, 
		 getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## If Inverse of matrix already exists in memory then return it
	inv <- x$getinverse()
    	if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
    	}
    
    ##Else Read the matrix, compute the matrix inverse, Cache the same & Return it
    data <- x$getmatrix()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
