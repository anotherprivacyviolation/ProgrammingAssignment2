## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

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
