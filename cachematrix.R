## This function works when called as follows

## functions do

## makeCacheMatrix creates a list of functions for transferring matrices
## and their calculated inverses between the local and another environment

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL  ##clear out old matrix inverse (this env)
        set <- function(y) {  #make function 
                x <<- y		#set x in other environment to y
                minv <<- NULL ##clear out old matrix inverse (other env)
        }
        get <- function() x  ##this function returns x, the parameter passed
        setinv <- function(invert) minv <<- invert ##this makes a function containing whatever function is passed to it
									##so we could probably still call it mean here
        getinv <- function() minv  ##this function returns the matrix inverse
        list(set = set, get = get,  ##put the four functions into a list
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve looks for an inverse of the matrix passed
##if there isnt one, it calculates, caches and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()  ##get mean from cache
        if(!is.null(m)) {  ##is there something there?
                message("getting cached data")
                return(m)  ##if yes, return it
        }
        data <- x$get() ##
        m <- solve(data, ...)
        x$setinv(m)
        m
}
