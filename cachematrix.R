# makeCacheMatrix takes one input:
#    m                   an invertible matrix
#
#  and returns a list of four functions:
#    set.matrix(m)       stores matrix and reset the inverse to NULL
#    get.matrix()        returns matrix
#    set.inverse(inv.m)  stores the inverse
#    get.inverse()       return cached inverse

makeCacheMatrix <- function(m = matrix()) {
    inv.m <- NULL

    # set the value of the matrix and reset inverse to NULL    
    set.matrix <- function(y) {
        m <<- y
        inv.m <<- NULL
    }

    # get the value of matrix
    get.matrix <- function() m

    # set the value of inverse
    set.inverse <- function(inverse) inv.m <<- inverse

    # get the value of inverse (if matrix did NOT change)
    get.inverse <- function() inv.m

    # return list of functions
    list(set.matrix = set.matrix, 
        get.matrix = get.matrix,
        set.inverse = set.inverse,
        get.inverse = get.inverse)
}

# cacheSolve finds the inverse of a matrix created using
# makeCacheMatrix. If the inverse has been found, the
# cached version of the inverse is returned; otherwise it
# will calculate the inverse of the matrix and cache it.

cacheSolve <- function(m, ...) {

    # get current version of the inverse
    inv.m <- m$get.inverse()

    # if inverse has been computed, return cached version
    if(!is.null(inv.m)) {
        message("Getting cached inverse...")
        return(inv.m)
    }

    # compute and cache inverse
    actual.matrix <- m$get.matrix()
    inv.m <- solve(actual.matrix)
    m$set.inverse(inv.m)

    # return inverse matrix
    inv.m
}
