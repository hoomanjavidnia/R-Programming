# ============================================
# = John Hopkins Data Science Specialization =
# = R Programming                            =
# = Week 3                                   =
# = Programming Assignment 2                 =
# = August 2015                              =
# = Hooman Javidnia                          =
# ============================================ 

# These are the two functions that are defined as part of Assignment 2 of R
# Programming Course on Coursera. This assignment is about lexical scoping.

# For this exercise I am following the methodology used in the example given as
# part of the assignment instructions. The idea is that when we are dealing with
# expensive calculation of the inverse of a large matrices, we don't want to 
# repeat this operation every time we encounter the inverse of the given matrix
# in the code and instead use a cached version of the inverse. The following
# two functions are achieving this goal. 

# The first function "makeCacheMatrix()" is going to work as a replacement for
# the general matrix class. Instead of a matrix, it creates a list of 4
# functions. These 4 functions are named:
# 1. set
# 2. get
# 3. set.inverse
# 4. get.inverse

# Initially, the assumption is that the inverse of the matrix is not calculated
# and doesn't exist. That is why it is initially set to NULL.

# makeCacheMatrix is a function that defines a list of 4 functions to create
# a matrix-like object with 4 methods for setting the matrix, getting the value
# of the matrix, setting the inverse of the matrix, and getting the matrix's
# inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        # If we call the set function on an object of type makeCacheMatrix, it 
        # means that matrix x that was originally used is changing. That is why
        # <<- assignment is used that not only assings the value of y to x in 
        # set matrix, but also,  changes it in other places too.
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    set.inverse <- function(inverse) inv <<- inverse
    
    get.inverse <- function() inv
    
    list(set = set, get = get, set.inverse = set.inverse,
         get.inverse = get.inverse)
}

# casheSolve takes an object of the type cacheMatrix and calculates its inverse
# if it is not cached before. If it is cached, it will return the cached
# version.
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
