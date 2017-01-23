## Put comments here that give an overall description of what your
## functions do
##Assignment Details: Matrix inversion is usually a costly computation and there may be some benefit to caching 
##the inverse of a matrix, rather than computer it repeatedly (there are also alternatives to matrix inversion
##that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCac heMatrix above.
##If the inverse has already been calculated (and the mnatrix has not changed), then the cacheSolve should retrieve
##the inverse from the cache.
## Write a short comment describing this function

##This is function 1: makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##This is function 2: it returns the inverse of the matrix after first checking to see
##if the inverse has already been computer. If so, it gets the cached result and skips the computation.
##If it hasn't been computed, it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("gettingcached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
