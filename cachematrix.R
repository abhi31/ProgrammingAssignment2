## this function will accept an object. If no object is passed to it, a default matrix with no content is used.
## creates anonymous functions named set - to set the value of x available in the out of anonymous function scope.
## get to retrieve the object x
## setInverseMat - anonymous function to store the inverse of the matrix x
## getInverseMat - anonymous function to retrieve the inverse of matrix x  
 
makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL
	set <- function(y) {
	  x <<- y
	  invMat <<- NULL
	}
	get <- function() x
	setInverseMat <- function(inverseMat) invMat <<- inverseMat
	getInverseMat <- function() invMat
	list(set = set, get = get, setInverseMat = setInverseMat, getInverseMat = getInverseMat)
}


## this function calculate the inverse of a matrix and cache the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	invMat <- x$getInverseMat()
	if(!is.null(invMat)) {
    	message("getting cached data")
    	return(invMat)
  	}
  	data <- x$get()
  	invMat <- solve(data, ...)
  	x$setInverseMat(invMat)
  	invMat
}
