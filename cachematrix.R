## Programming Assignment 2
## Provide the inverse of a matrix, using a cache so that 
## we don't have to recalculate each time.
##
## EW June 2021

## Provide get/set access functions for a given matrix 
## so that values can be cached.
## returns  - List with elements/functions
##              - set() & get() for the matrix
##              - setInverse() & getInverse() for the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    cacheInverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        cacheInverse <<- NULL
    }
    get <- function() x
    getInverse <- function() cacheInverse
    setInverse <- function(newInverse) cacheInverse <<- newInverse
    list(get = get, set = set, 
         getInverse = getInverse, setInverse = setInverse)
}


## Return the inverse of a matrix, using the cached value if possible
## cm    - a list of access functions for matrix, provided by makeCacheMatrix()
cacheSolve <- function(cm, ...) {
    ## WIBNI check cm is valid, before we start doing anything
    if (is.null(cm$getInverse())) {
        ## No cached value, so need to calculate & store it
        message("Note - calculating new inverse")
        cm$setInverse(solve(cm$get()))
    }
    cm$getInverse()
}
