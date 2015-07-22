# This R function is supposed to cache potentially time-consuming computations,
# such as matrix inversions and look up the value in the cache, when it is
# called. It returns the cached value if the matrix has not be changed, else it
# computes the new matrix inversion.

# makeCacheMatrix() creates a special "matrix", and certain callable functions
# that enables caching (storing) and recall of information.

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x

    # Could also use ginv from MASS::ginv.
    setInvMat <- function(solve) m <<- solve
    getInvMat <- function() m

    # Returns a list with the following formal argument and parameters
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}

# cacheSolve() calculates the inverse of a special "matrix" created with
# the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and
# skips the computation. Otherwise, it calculates the inverse of the data
# and sets the value of the inverse in the cache via the setInvMat function.

cacheSolve <- function(x, ...)
{
    # Return a matrix that is the inverse of 'x'
    m <- x$getInvMat()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()

    # Calculating the new inverse of matrix
    invMat <- solve(data, ...)
    x$setInvMat(invMat)

    # Returns the inverse of 'x'
    invMat
}
