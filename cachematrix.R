## Put comments here that give an overall description of what your
## functions do
## The function pair will return the inverse of a matrix given. 
## Before doing so, the program will check if the same matrix has
## been processed before in the cache.
##
## Write a short comment describing this function

## Below function is to return a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function (inverse) inv <<- inverse
	getinverse <- function () inv
	matrix(list(set, get, setinverse,
		getinverse),nrow = 2, ncol =2)
}


## Write a short comment describing this function
## Below function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x[2,2]()
	  if (!is.null(inv)){
	  		message("getting cached data")
			return (inv)  
	  }
	  mat <- x[1,2]()
	  inv <- solve(mat,...)
	  x[2,1](inv)
	  inv

}
