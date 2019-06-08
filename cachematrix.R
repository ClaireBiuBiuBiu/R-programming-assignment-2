
#Assignment 2 for week 3, Lexical Scoping#


# This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()){
    m <- NULL
    set <- function(y){   #  set the value of the matrix
        x <<- y 
        m <<- NULL
    }
    get <- function() x  # get the value of the matrix
    setInverse <- function(inverse) m <<- inverse  # set the value of inverse of the matrix
    getInverse <- function() m  # get the value of inverse of the matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Thxis cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed) 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cache data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...) ## If X is a square invertible matrix, then solve(X) returns its inverse.
    x$setInverse(m)
    m
}
