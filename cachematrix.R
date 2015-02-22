## The two functions below (makeCacheMatrix and cacheSolve) implements
## a cached version of matrix inversion.

## Caching matrix inversion is important as it is a costly computation
## when the matrix is large.

## makeCacheMatrix creates a special matrix object that can store a
## normal matrix as well as a cached inverse. Setter and getter functions
## allows setting the original matrix, while setinverse and getinverse
## functions will allow updating and retrival of the cached inverse matrix.
## Before using the inverse functions, it is important to call cacheSolve 
## to perform a one-time computation to compute the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	## Initialise inverse matrix (inv) to NULL at start
	inv <- NULL

	## Create setter function with argument y for setting matrix x
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## Create getter function to return matrix x
	get <- function() x

	## Create setinverse function to set the inverse matrix
	setinverse <- function(inversex) inv <<- inversex

	## Create getinverse function to get the cached inverse matrix
	getinverse <- function() inv

	## Create list structure to hold functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve function gives the inverse of the special cache matrix object x
## It firstly tries to get a cached result, but if it is not available, a
## computation is performed using the normal solve() function and cached.

cacheSolve <- function(x, ...) {
    ## Attempt to get cached inverse matrix
    inversex <- x$getinverse()
    if(!is.null(inversex)) {
        message("getting cached inverse matrix")
        return(inversex)
    }
    
    ## If we're here then cache missed. Compute inverse, store & return
    data <- x$get()
    inversex <- solve(data, ...)
    x$setinverse(inversex)
    inversex
}
