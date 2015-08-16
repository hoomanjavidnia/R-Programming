# These are the two main functions that are defined as part of Assignment 2 of R
# Programming Course on Coursera. This assignment is about lexical scoping.
# From all that I have read on the forum, it seems that this is more like an 
# exercise and does not have practial usage at least in the form as is.

# For this exercise I am following the methodology used in the example given as
# part of the assignment instructions.

# The first function "makeCacheMatrix()" is going to work as a replacement for
# the general matrix class. Instead of a matrix, it creates a list of 4
# functions. These 4 functions are named:
# 1. set
# 2. get
# 3. set.inverse
# 4. get.inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    set.inverse <- function(inverse) inv <<- inverse
    
    get.inverse <- function() inv
    
    list(set = set, get = get, set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$get.inverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inverse(inv)
    inv
}
