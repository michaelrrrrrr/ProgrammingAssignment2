## The function makeCacheMatrix stores an invertible matrix 
## and its corresponding inverse. The user can set its inverse 
## manually by invoking the function setInv.
## Variables:
##   x: refers to an invertible matrix
##   inv: refers to the inverse of x

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, 
		setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## The function cacheSolve returns an inverse of a special matrix 
## x, which is created by the function makeCacheMatrix. 
## If object x does not have its inverse available, the function
## cacheSolve calculates x's inverse and stores it in object x.
## Variable:
##   x: refers to a matrix created by invoking the function 
##      makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
