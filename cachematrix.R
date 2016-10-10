## Cached matrix

## example:
## x<-makeCacheMatrix(matrix(rnorm(100),10,10))
## cacheSolve (x)

## function: creates a matrix object to invert
## usage: x<-makeCacheMatrix(myMatrix)
makeCacheMatrix <- function(x=matrix()) {
        i <- NULL
        set <- function(y) {
                y<-matrix(y)
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## function: inverts a matrix and places result in cache
## usage: cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}