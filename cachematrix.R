# Coursera R Programming Course 005 - Programming Assignment 2
# Identity of author withheld as per course request.

# EXPLANATION:
# The purpose of this file is to read a matrix into a function that calculates 
# the inverse of that matrix.  The first function "makeCacheMatrix" caches the 
# matrix so that its inverse may be called later.  The second function 
# "cacheSolve" checks if the matrix is already cached and either calls the 
# value from the cache, or recognizes that the inverse of that matrix is not
# in the cahce and returns the inverted matrix.

# NOTE: We assume that every inputted matrix is invertible.  This program
# is not designed to solve non-invertible matrices.

# To run, simply declare a matrix in R, then define a new variable and assign 
# it the value of makeCacheMatrix with the matrix inputted into the function.
# This caches the inverted matrix, which can then be retrieved by calling the
# cacheSolve function inputted with the variable assigned to makeCacheMatrix.

# For example, you may run the code:
# m <- matrix(c(1,4,3,-1), 2:2)
# v <- makeCacheMatrix(m)
# cacheSolve(v)

# Function 1: Caching an inputted matrix
makeCacheMatrix <- function(x = matrix()) {
	
	# Setting an empty variable to return from the setInverse function
	z <- NULL

	# Setting the value to be passed to the get function
	set <- function(y) {
		x <<- y
		z <<- NULL
	}

	# Getting the inputted matrix
	get <- function() x

	# Solving and setting the inverse in the cache
	setInverse <- function(solve) z <<- solve

	# Retrieving the inverse
	getInverse <- function() z
	
	# Setting variables to appropriate calling values
	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}

# Function 2: Checking if the matrix is cached, or solving its inverse
cacheSolve <- function(x = matrix(), ...) {

	# Retrieving the value of getInverse if previously run
	z <- x$getInverse()
	
	# Checking if the inverse is cached	
	if(!is.null(z)) {
		message("getting cached data")
		return(z)
	}

	# Solving the inverse problem
	data <- x$get()
	z <- solve(data, ...)
	x$setInverse(z)
	z

}
