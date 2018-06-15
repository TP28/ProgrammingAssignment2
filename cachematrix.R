
### Programming Assignment 2 - TP28 ###


# store matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {       # set the value of the vector
                x <<- y
                i <<- NULL
        }
        get <- function() x              # get the value of the vector
        setinverse <- function(Inverse) i <<- Inverse #set the value of the mean
        getinverse <- function() i          #get the value of the mean
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# calculate inverse
cacheInverse <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")   #show this if inverse is already calculated
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


