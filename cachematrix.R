## The two functions written here, makeCacheMatrix and cacheSolve, are used to create
## a special object to store a matrix and cache its inverse (using solve) and then recall
## that cached inverse (to avoid recalculating it).

## The first function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse (using solve)
## 4. get the value of the inverse (using solve)

makeCacheMatrix <- function(x = matrix()) {

        ii <- NULL
        set <- function(y) {
                x <<- y
                ii <<- NULL
        }
        get <- function() x
        setinv <- function(solve) ii <<- solve
        getinv <- function() ii
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function calculates the inverse of the special "vector" created 
## with the above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ii <- x$getinv()
        if(!is.null(ii)) {
                message("getting cached data")
                return(ii)
        }
        data <- x$get()
        ii <- solve(data, ...)
        x$setinv(ii)
        ii
}
