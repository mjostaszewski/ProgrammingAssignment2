## The two functions handle caching of the inverse of a given matrix
## First, a "makeCacheMatrix" object needs to be created with a matrix as an input.
## the initial inverse for a new "makeCacheMatrix" object is NULL, also setting a new metrix resets the inverse to NULL. 
## 
## Second, cacheSolve can be run on the "makeCacheMatrix" object.
## The function checks, if the inverse needs to be calculated, and modifies the "makeCacheMatrix" object accordignly.
## 

### Container function for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	#The inverse will be stored here
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() { x }
	setinverse <- function(new_inv) { inv <<- new_inv }
	getinverse <- function() { inv }
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function calculates the inverse of the matrix stored in the "makeCacheMatrix" container
## Although in the assignment we have to assume that the matrix is invertible, I've added a small error-handling piece based on try() function.
## if verbose is set to TRUE, a possible error output will not be suppressed
cacheSolve <- function(x, verbose = F, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	message("No cached inverse for this matrix found, calculating...")
	### Try to solve the inverse (it may fail)
	inv <- try(solve(x$get(), ...), silent = !verbose)
	### If not solved, print a warning and set inv to NULL
	if(class(inv) != "matrix") {
		message("I couldn't calculate the inverse of this matrix.")
		if(!verbose) { message("Please, run cacheSolve with verbose = T to get more information") }
		inv <- NULL
	}
	x$setinverse(inv)
	return(inv)
}

## For instance, you may try:
## Set the test matrix and the corersponding "makeCacheMatrix" object

# test <- 1:5*diag(1,5)
# mc_test <- makeCacheMatrix(test)
# mc_test$getinverse()

## Run the "cacheSolve" for the first time
# cacheSolve(mc_test)
# mc_test$getinverse()

## Run the "cacheSolve" for the second time, cache is used
# cacheSolve(mc_test)
# mc_test$getinverse()

## Set another matrix in the mc_test and re-run the "cacheSolve", cache is empty and the inverse is calculated
# mc_test$set(1:7*diag(1,7))
# cacheSolve(mc_test)
# mc_test$getinverse()