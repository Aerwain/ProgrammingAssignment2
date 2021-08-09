## The following functions cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse matrix
        setinv <- function(inverse) m <<- inverse
        #get the value of the inverse
        getinv <- function() m
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolvecomputes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
        ##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


#Testing
A <- matrix(c(1,2,3,4),2,2)
A1<-makeCacheMatrix(A)
cacheSolve(A1)
