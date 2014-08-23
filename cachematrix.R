## These functions together are able to take one given matrix and computes
## its inverse. This, as long as you give the functions an inverse matrix of 2x2
## A good example would be matrix(c(4, 1, 3, 1), 2, 2). They also manage to cache 
## all the information on a special matrix, wish contains a list of functions. 
## This functions are able to set values to the first matrix and its inverse, and also 
## to stores it, so you can get it back latter without doing again the calculation. 

## This function creates an especial "matrix" wish contains a list of functions
## As well as the "makeCacheMatrix()" function, wish needs a matrix of 2x2, also
## the "set()" and "setinverse()" functions need the input to be a matrix of 2x2.
## This "makeCacheMatrix()" function won't do other than cache information,
## the "x" matrix (at the "get()" function), or the inverse of "X" (at the 
## "getinverse()" function). 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(newinverse = matrix()) inverse <<- newinverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The "cacheSolve()" function takes an special object created by the "makeCacheMatrix()"
## function and 1) Reads the information of the "getinverse()" function. 2) If this information
## it´s not "NULL" then it would print a massage and the information of the "getinverse()" function.
## 3) If the information was "NULL", then it would read the information of the "get()" function
## and stores the information on "data". 4) Then it would computes the inverse of "data" (with the solve() function)
## and stores it at "inverse". 5) This function would also save this new information on the object "x", 
## on its "setinverse()" function (then object "x" would cache the inverse on "getinverse()").
## 6) Finally, it would print the inverse of "x". 


cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- matrix() 
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
