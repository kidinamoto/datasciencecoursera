## creator: Suyu HUANG
## date : 2014/8/22 
## version 1

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #  m will store our 'inverse' and it's reset to NULL every 
                    #    time makeCacheMatrix is called
        
        set <- function(y) {    # this is called by cacheSolve() during the first cacheSolve()
                x <<- y         #  access and it will store the value using superassignment
                inv <<- NULL
        }
        
        get <- function() { x }    # this function returns the original matrix
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() {inv}  # this will return the cached value to cacheSolve() on
                                       #  subsequent accesses
        
        list(set = set, get = get,      #  This list is returned with the newly created object. 
             setinverse = setinverse,   #   It lists all the functions ("methods") that are part of
             getinverse = getinverse)   #   the object.  If a function is not on the list then it cannot
}                                       #   be accessed externally.


## this is the function for calculating the inverse of the matrix

cacheSolve <- function(x, ...) { # the input is an object created by makeCacheMatrix
        
        inv <- x$getinverse()    # accesses the object 'x' and gets the value of the inverse
        
        if(!is.null(inv)) {
                print("getting cached data")
                #message("getting cached data")
                return(inv)
        }
        else{
                print("not getting cached data")
        }
        
        data <- x$get() # we reach this code only if x$getinverse() returned NULL
        
        inv <- solve(data, ...) #calculate the inverse
        
        x$setinverse(inv)        # store the calculated inverse value in x
        
        inv  ## Return a matrix that is the inverse of 'x'
}
