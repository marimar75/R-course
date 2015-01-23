## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" to cache the inverse matrix
## Ex : 
##	x <- matrix(1:4,2,2)
##	z <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
	 s <- NULL
       set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve ,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
## 1. Look first for cached data if exists
## 2. Else calculate inverse of 'x' and cache result for next time
## Ex : 
##	z <- makeCacheMatrix(x)
##	s1 <- cacheSolve (z)	## --> 1st time : calculate inverse and cahe it
##	s2 <- cacheSolve (z)	## --> 2nd time : get cahed data

cacheSolve <- function(x, ...) {
       
	  s <- x$getsolve ()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve (s)
        s
}
