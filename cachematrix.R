## This is the function that produces the object that can accepted into 
## CacheSolve where inverse of matrix is computed.

## On calling the function it produces a list containing the four functions, 
## set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
 	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is the function that will be called to compute the inverse
## If the inverse if already computed, it will return cached data, else
## it would compute the inverse in "m <- solve(data)" and the next time
## the function is called, cached data is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)

        x$setinverse(m)
        m
}
