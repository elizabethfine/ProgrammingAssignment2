## The goal is to calculate the inverse of the matrix and than to cache it.
## In order to do this, makeCacheMatrix() and cacheSolve() are used.
## makeCacheMatrix() obtains the matrix of which the inverse is
## calculated. cacheSolve() returns the inverse of matrix

## makeCacheMatrix() obtains the matrix. Inside the function,
## functions get(), set(), setinverse() and getinverse() are
## defined and these are placed in a list. Thus, makeCacheMatrix()
## rerturns the list of functions.

makeCacheMatrix <- function(x = matrix()) {
 	
	i <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inv) i <<- inv
        
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve() obtains the list of functions returned by
## makeCacheMatrix(). It first checks if there exists
## already calculated inverse matrix. If it does, cacheSolve()
## returns this cached inverse. If it doesn't exist, cacheSolve()
## calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 	i <- x$getinverse()
        
        # if i isn't empty, i.e. if there is inverse of matrix stored in i, return i
        if(!is.null(i)) {
                message("getting cashed inverse")
                return(i)
        }
        
        # if i is empty, calculete inverse and return it
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
