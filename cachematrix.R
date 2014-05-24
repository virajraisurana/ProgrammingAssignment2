## The two functions collectively perform the activity of taking a square, non-singular matrix as input and calcualting and caching its inverse.

## Function to create a list of functions to cache the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){	
		x <<- y                         
		inv <<- NULL            ##Resetting to NULL each time the input matrix is changed
	}

	get <- function() x		

	setinv <- function(c) inv <<- c         ##Caching inverse

	getinv <- function() inv		

	list(set = set, get = get,	##Setting alias names of the functions
             setinv = setinv,
             getinv = getinv)
	

}



## Function to calculate the inverse of a matrix. If the inverse is already cached, it returns the cached value.
## Assumption here that the input matrix is a square, non-singular matrix.

cacheSolve <- function(x) {
        
	inv <- x$getinv()
        if(!is.null(inv)) {   ##Checking if the inverse is cached
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
        inv <- solve(data)      ##Calculating inverse
        
        x$setinv(inv)           
        
        inv
}
