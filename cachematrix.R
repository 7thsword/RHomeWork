##A pair of functions that cache the inverse of a matrix 
 

##Create a special "matrix" object that cache the inverse of the inputed matrix 
makeCacheMatrix <- function(x = matrix()) {
	matrix <- NULL
	
	set <- function(y){
		x <<- y
		matrix <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverse) matrix <<- inverse

	getInverse <- function() list(original=x,cache=matrix)

	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


##Compute the inverse of the sepcial matrix returned by makeCacheMatrix above.
##If the inverse has already been calculated and the matrix has not changed,
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      temp <- x$getInverse()
	matrix <- temp["cache"]
	data <- x$get()
	if( all(dim(data)==dim(temp["original"])) && all(data==temp["original"]) &&!is.null(matrix)){
		message("Getting cached data")
		return(matrix)
	}  
	
	
	matrix <- solve(data,...)
 	x$setInverse(matrix)
	matrix
	## Return a matrix that is the inverse of 'x'
}
