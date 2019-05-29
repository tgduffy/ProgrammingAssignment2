## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). The first function in the file, makeCacheMatrix()
# creates an R object that stores a matrix and its inverse. The second function, cacheSolve() requires an argument that is returned by 
#makeCacheMatrix() in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

## makeCacheMatrix builds a set of functions and returns the functions within a list to the parent environment

makeCacheMatrix <- function(x = numeric()) {
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


## cacheSolve is required to populate or retrieve the mean from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        print(data)
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Code for testing
# aMatrix<-makeCacheMatrix(matrix(rnorm(25,5,2), ncol=5))
# aMatrix$get()
# cacheSolve(aMatrix)
