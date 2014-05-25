## These functions use a separate environment to cache a matrix and its'
## inverse to save time when repeated calculations of the inverse are needed.
## When the inverse is first computed it is stored in the cache so that any
## subsequent calculation can be done by accessing the already cached result.


## This function creates a matrix cache, with room for a matrix and its'
## inverse. The resulting cache can be accessed via getx, setx, setxInv,
## and getxInv using for example MCache$getx(), where MCache is an already
## created cache.

mmakeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        setx <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        getx <- function() x
        setxInv <- function(I) xInv <<- I
        getxInv <- function() xInv
        list(setx = setx, getx = getx,
             setxInv = setxInv,
             getxInv = getxInv)


}



## Once the previous function has been used to create a cache for a matrix
## M the following function returns the inverse of M by returning the cached
## value of the inverse if it has already been computed. Otherwise it
## computes the inverse, caches it, and then returns it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getxInv()
        if(!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
        }
        data <- x$getx()
        xInv <- solve(data, ...)
        x$setxInv(xInv)
        xInv

}
