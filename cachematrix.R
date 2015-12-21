## These are functions which will cache the inverse of a matrix

## The makeCacheMatrix is a function that creates a special "matrix"
## object that can cache its inverse.

## To make it work, assign a square matrix (same number of rows and columns) to a variable, e.g., saveMatrix
## Create a new variable with the assignment of makeCacheMatrix(savedMatrix)
## Feed this new variable to cacheSolve.
## Then by just running cacheSolve, you will get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) i <<- solve
    getmatrix <- function() i
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getmatrix()
        if(!is.null(i))  {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setmatrix(i)
        i
}
