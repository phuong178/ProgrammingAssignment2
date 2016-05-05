## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_x <<- inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached matrix")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$setinverse(inv_x)
    inv_x
}

## creates a special "vector", which is really a list containing a function to
#   set the value of the vector
#   get the value of the vector
#   set the value of the mean
#   get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
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

## The following function calculates the mean of the special "vector" created with the above function.
# However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function
cachemean <- function(x, ...) {
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