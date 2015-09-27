## Put comments here that give an overall description of what your
## functions do

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the
##    inverse from the cache.

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
        set <- function(y) {
            x <<- y
                inv <<- NULL
        }
    get <- function() x
        setinverse <- function(invmatrix) inv <<- invmatrix
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated.  If so , it gets the inverse from
## the cache and skips the computation.  Otherwise, it calculates the inverse of the data and set the value
## of the inverse in the cache via the setinverse function
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
        if (!is.null(inv)) {
            message("getting cached data")
                return (inv)
        }
    data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv

}
