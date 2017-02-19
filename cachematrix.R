## Since re-computing the inverse of a matrix in a loop costs so much computational time, this pair of functions
## caches the result once, then uses that result the next time, rather than re-calculating it.

## 1st function builds list of functions to cache and retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL ## define matrix inverse cache
        set <- function(y) {
                x <<- y ## assign matrix y to parent environment variable x
                m <<- NULL ## re-initialize matrix cache
        }
        get <- function() x ## return cached matrix
        setinverse <- function(inverse) m <<- inverse ## cache matrix inverse
        getinverse <- function() m ## return cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## 2nd function calculates the matrix inverse again only if not already cached.

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
