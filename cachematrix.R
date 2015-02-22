## Have 2 functions in this file makeCacheMatrix and cacheSolve


## This function stores the 4 function definitions as a list object

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	setmatrix <- function(y) {
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


## This function calls the different matrix related functions stored in a list (as seen in the ## above function)

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
