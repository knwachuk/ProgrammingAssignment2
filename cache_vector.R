# This R function is supposed to cache potentially time-consuming computations.
# concept is to look up in a cache, rather than recompute a non-changing
# vector (or matrix) value.

# The function, make_cache_matrix, is to

make_cache_vector <- function(x = numeric())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# Write a short comment describing this function

cache_vector_solve <- function(x, ...)
{
    # Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
