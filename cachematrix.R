## Put comments here that give an overall description of what your
## functions do

## Functions are to show benefit to caching the inverse of a matrix
## rather than computing it repeatedly. The functions cache the inverse
## of a matrix

## Write a short comment describing this function
## This function creates a special matrix object, which is really a lit 
## containing a function to set the value of the matrix, get the value
## of the matrix, set the value of the inverse, and get the value of 
## the inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, 
	     setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The function calculates the inverse of the special "matrix" object
## created with the first function. It first checks to see if the mean has
## already been calculated. If so, it get the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of matrix in the cache via "setinverse" function.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
