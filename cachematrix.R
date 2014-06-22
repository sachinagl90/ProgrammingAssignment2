## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
    { ##create a function which starts with a null matrix argument
        matrixinverse <- NULL ## initialize the value of the matrix inverse to NULL
        set <- function(y) { ## delcare another function set where the value will be cached in 1. Matrix is created
        ## for the first time. 2. changes made to cached matrix
        x <<- y
        matrixinverse <<- NULL ## change the value of inverse of the matrix in case the matrix was changed.
        }
    get <- function() x ## gets the value of the inverse
    setinverse <- function(solve) matrixinverse <<- solve #calculates the inverse of non-singular matrix via the solve function
    getinverse <- function() matrixinverse # gets the inverse
    list(set = set, get = get, ## passes the value of the function makeCacheMatrix
    setinverse = setinverse,
    getinverse = getinverse)
    }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
##the inverse from the cache

cacheSolve <- function(x, ...) 
    { # used to get the cache of the matrix
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse))
            { #if the inverse exists, it gets it.
                message("getting cached data - Inverse of the matrix")
                return(matrixinverse)
            }
        data <- x$get() #if the inverse if not there, first it is calculated and then retrieved.
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
    }
