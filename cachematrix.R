## usage: 
## 1. defined a matrix : x <- matrix(n,k,k)
## 2. make the cacheMatrix: cx <- makeCacheMatrix(x)
## 3. then: cacheSolve(cx) to calc its inverse, if recalc, fetch results from cache

## set/get a matrix ,cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y) {
		x <<- y
		invx <<- NULL
}
	get <- function() x
	setinvx <- function(solve) invx <<- solve
	getinvx <- function() invx
	list(set=set, get=get, setinvx=setinvx, getinvx=getinvx, getprvx=getprvx)
}

## get from cache or Solve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invx <- x$getinvx()
	data <- x$get()
	
	if (!is.null(invx) ) {
		message("getting cached data")
		return(invx)
	}
	invx <- solve(data,...)
	x$setinvx(invx)
	invx
}

