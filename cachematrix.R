## Two functions for creating and reading a matrix-inversion. A matrix inversion
## is, depending on size, potentially a time consuming operation Before 
## calculating an inversion these functions check if the inverted matrix already
## is calculated. If so we use 'the cached data', if not we calculate and cache 
## the inverted matrix for future use.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #create empty m
        set <- function(y) { #set the value to the contents of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x #get (read/display) the contents. 
        #if set has been applied, this returns a matrix
        setinverse <- function(solve) m <<- solve #calculate inverted matrix
        getinverse <- function() m #read inverted matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` should retrieve the inverse from 
## the cache. If matrix has changed (i.e. 'makeCacheMatrix was reapplied) a new
## enviroment is created and a new inverse value has to be calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #get inverse value from x
        
        if(!is.null(m)) { 
                #if inverse value is present, then get cached data
                message("getting cached data")
                return(m)
        } else { 
                #inverse not available, calculate inverse using solve
                data <- x$get() #get matrix data from x
                m <- solve(data, ...) 
                x$setinverse(m)
                m # return inversed matrix (which is in x$setinverse and m)
        } #end else
}
