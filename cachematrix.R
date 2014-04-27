## Catching the Inverse of a Square Matrix

## Function makeCacheMatrix creates a special Matrix, which in fact is a list of 4 Elements: 
## 1. Set the Matrix; 2. Get the Matrix; 3. Set the Inverse of the Matrix; 4.Get the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve  ## Implement the Inverse matrix function to the created matrix
        getInverse <- function() s
        list(set = set, get = get,     ## This function return a list of these 4 elements
             setInverse = setInverse,
             getInverse = getInverse)

}


## The following function calculates the inverse of the special square matrix created in the above functions.
## First it checks if the inverse has been already calculated, allowing to skip further computation.  Otherwise, it calculates the Inverse of the matrix and sets the value in the cache

cacheSolve <- function(x, ...) {
        s <- x$getInverse()      ##query the x matrix's cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)        ## if the function finds the cache, just returns his value, no computation needed
        }
        data <-x$get()           ##else, if there's no cache, we do:
        s <- solve(data, ...)    ## Computing the matrix here
        x$setInverse(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
