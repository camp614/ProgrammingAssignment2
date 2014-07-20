## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. The following two functions can be used 
## to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	# set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# get the value of the matrix
	get <- function() x
	
	# set the value of the inverse
	setinv <- function(inverse) inv <<- inverse
	
	# get the value of the inverse
	getinv <- function() inv
	
	list(set=set, get=get, setinverse=setinv, getinverse=getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then it is rerieved from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	
	# return the cached inverse (if it exists)
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# calculate the inverse
	data <- x$get()
	inv <- solve(data, ...)
	
	# cache the inverse
	x$setinv(inv)
	
	inv
}
