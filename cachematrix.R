# Assignment 2.
#
# Use the "environment" to store and manage the results of
# an expensive calculation. Explore Scoping and caching. 

## Thanks and Acknowledgements
#
# I give great thanks to the people posting in the discussion area! 
#
# The R Project, Khan Academy, and  YouTube Tutorlol were all a great help:
# R Projects R-intro.pdf, with "<<-" operator explanation
#   http://cran.r-project.org/doc/manuals/R-intro.pdf 
# Kahn Academy explains an identity matrix
#   https://www.khanacademy.org/math/precalculus/precalc-matrices/zero-identity-matrix-tutorial/v/identity-matrix
# Tutorlol shows how to use the solve() function to return an inverse of a matrix
#   https://www.youtube.com/watch?v=9kImnwZHQyc

## Background
#
# From the R-intro.pdf:
# "10.5 Assignments within functions
# Note that any ordinary assignments done within the function are local and temporary and are
# lost after exit from the function. Thus the assignment X <- qr(X) does not affect the value of
# the argument in the calling program.
# ....
# If global and permanent assignments are intended within a function, then either the “superassignment”
# operator, <<- or the function assign() can be used."

## Notes on call used
#
# The R solve() function accepts three parameters, and "... further arguments 
# passed to or from other methods." When taking the inverse of a matrix, only
# one parameter is used. The second parameter defaults to the identity matrix, and 
# others have no relevance to caching the inverse.

## Usage example from the class discussion area:
#
#  Here is a matrix with a known inverse:
#    matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
#
# The inverse of this matrix is:
# -24  18   5
#  20 -15  -4
#  -5   4   1
#
# Use the functions makeCacheMatrix and cacheSolve
# The second call returns the cached value for the inverse:
#    exampleMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
#    matrixVector <- makeCacheMatrix(exampleMatrix)
#    matrixVector$get()
#    cacheSolve(matrixVector)
#    cacheSolve(matrixVector)

# Given an invertible matrix x, cache the inverted matrix m
# Example of R caching (the environment is used)
makeCacheMatrix <- function(x = matrix()) {
    # Build functions and store them is a list. The functions cache and 
    # return an inverted matrix.
        m <- NULL
        set <- function(y) {
                x <<- y                                 # The “superassignment” operator "saves"
                m <<- NULL                              # by changing the scope of the values
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve       # Again, bump up the scope
        getinverse <- function() m
        list(set = set, get = get,                      # The list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x) {
        # Given an invertible matrix x, calculate and then cache the
        # inverted result
        m <- x$getinverse()                              # Get the inverted matrix (if it exists)
        if(!is.null(m)) {                                # If it exists, note it and use it
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        return(m)                                       # I like the explicit return
}
