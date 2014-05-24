#This function takes an invertible matrix and computes and stores its inverse. 

makeCacheMatrix <- function(x = matrix()) {
				inv <- NULL
				set <- function(y) {
				x <<- y
				inv <<- NULL
				}
				get <- function() x
				setinv <- function(solve) inv <<- solve
				getinv <- function() inv
				list(set = set, get = get, 
					setinv = setinv, 
					getinv = getinv)
}



#This function computes the inverse of a matrix. If the matrix is already stored 
#in the above function, it reads the inverse from there. Otherwise, it computes
#the inverse, returns the result and also updates it in the above function.

cacheSolve <- function(x, ...){
	inv  <- x$getinv()
	if(!is.null(inv)){
		 message("getting cached inverse")
		 return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv
	inv
}
