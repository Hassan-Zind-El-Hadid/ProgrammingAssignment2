## These functions are used for caching the inverse of a matrix for later use.
## Rather than computing the inverse every time it is needed, we cache it and retrieve it when needed


## makeCacheMatrix is a function that creates an R object that stores a matrix and its inverse.
## It can be used to instantiate an object that returns a set of 4 functions in a list and
## two data objects, x and i

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve requires an argument from makeCacheMatrix to retrieve the inverse of the matrix
## from the cache stored in makeCacheMatrix environment. If the inverse was never calculated and
## stored in the cache, this function will calculate the inverse and store it within makeCacheMatrix
## environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message('getting cached data')
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
