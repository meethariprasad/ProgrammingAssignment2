## Put comments here that give an overall description of what your
## functions do

## A short comment describing makeCacheMatrix function

## makeCacheMatrix will return vector of 4 functions
## set: set the value of the vector
## get: get the value of the vector
## setsolve: set the value of the solve
## getsolve: get the value of the solve

makeCacheMatrix <- function(x = matrix()) {

##assign m, which will hold solve valuem to Null within the scope of makeCacheMatrix function
 m <- NULL
 ##set the function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##get function will get the value of matrix passed to function
        get <- function() x
        ##set solve will set the value of solve variable m 
        setsolve <- function(solve) m <<- solve
        ##get solve will get the value of function from global scope. If it has been not set, it will be NULL.
        getsolve <- function() m
       ##Setting list with values and label assignments
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function will return the solved matrix for input.
##In case it finds already solved matrix it will return from assigned.

cacheSolve <- function(x, ...) {
        ## Assign m with solve value in local scope by calling getsolve function from x
        m <- x$getsolve()
        ##In case m is not Null, it means we already have the solve matrix. Return the same.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Matrix data is obtained from get function and assigned to data variable
        data <- x$get()
        ##solve function is called to analyze the data
        m <- solve(data, ...)
        ##solved value is set in m and assigned in global scope via setsolve function definition 
        x$setsolve(m)
        
        m
}
