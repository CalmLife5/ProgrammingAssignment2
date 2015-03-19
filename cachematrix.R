# Assignment 2.
#
# Use the "environment" to store and manage the results of
# an expensive calculation. Explore Scoping and caching. 

## Thank yous and Acknowledgements
#
# I give great thanks to the people posting in the discussion area! 
#
# Both the r-project R-intro.pdf and the Khan Academy were also a great help:
# http://cran.r-project.org/doc/manuals/R-intro.pdf 
# https://www.khanacademy.org/math/precalculus/precalc-matrices/zero-identity-matrix-tutorial/v/identity-matrix

# From the r-project R-intro ( found at http://cran.r-project.org/doc/manuals/R-intro.pdf )
# 10.5 Assignments within functions
# Note that any ordinary assignments done within the function are local and temporary and are
# lost after exit from the function. Thus the assignment X <- qr(X) does not affect the value of
# the argument in the calling program.
# ....
# If global and permanent assignments are intended within a function, then either the “superassignment”
# operator, <<- or the function assign() can be used.

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
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x) {
        # Given an invertible matrix x, calculate and then cache the
        # inverted result
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

exampleMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
matrixVector <- makeCacheMatrix(exampleMatrix)
matrixVector$get()
cacheSolve(matrixVector)
cacheSolve(matrixVector)