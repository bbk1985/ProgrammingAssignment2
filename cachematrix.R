## Put comments here that give an overall description of what your
## functions do
## The objective of these functions are to create an inverse matrix and store it for later use
## Write a short comment describing this function
## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(oMatrix = matrix()) {
	invMatrix <- NULL
	
	set <- function(nMatrix) {
		oMatrix <<- nMatrix
		invMatrix <<- NULL
	}
	
	getOMatrix <- function() oMatrix
	
	setInvMatrix <- function(nInvMatrix) invMatrix <<- nInvMatrix
	getInvMatrix <- function() invMatrix
	
	list(set = set, getOMatrix = getOMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix);
}

## Write a short comment describing this function
## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##				If the inverse has already been calculated (and the matrix has not changed), 
##				then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		oInvMatrix <- x$getInvMatrix()
		if (!is.null(oInvMatrix)) {
			message("using cached data")
			return(oInvMatrix)
		} else {
			oMatrix <- x$getOMatrix()
			oInvMatrix <- solve(oMatrix)
			x$setInvMatrix(oInvMatrix)
			return(x$getInvMatrix())
		}
}
