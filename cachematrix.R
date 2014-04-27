## This method creates a matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) i  <<- inverse
        getinverse  <- function() i
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
}


## This method calculates the inverse of an invertible matrix.
##If it has already been calculated the method
## returns the stored inverse value from the cache

cacheSolve <- function(x, ...) {
        i  <- x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinverse(i)
        i
}
