## MakeCacheMatrix function will create an matrix object and capture its inverse
##
## cacheSolve function will accept the matrix returned by MakeCacheMatrix and check if 
##            inverse has already calculated and cached in MakeCacheMatrix. If inverse matrix already exists
##            in the cache it will be retireved else it will be captured in the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMat <- function () x
    setInv <- function(inversemat) inv <<- inversemat
    getInv <- function () inv
    list(setMat=setMat,getMat=getMat,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$getMat()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
