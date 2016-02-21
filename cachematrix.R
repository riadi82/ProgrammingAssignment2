## This two functions below is used to cache the inverse of a matrix,
## because matrix inversion usually a costly computation.


## Makecachematrix function create a special "matrix" object that will
   cache it's inverse 

makeCacheMatrix <- function(A = matrix()) {
        cm <- NULL
        set <- function(B) {
                A <<- B
                cm <<- NULL
        }
        get <- function() A
        setInverse <- function(inverse) cm <<- inverse
        getInverse <- function() cm
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## The next function process the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(A, ...) {
        cm <- A$getInverse()
        if (!is.null(cm)) {
                message("getting cached data")
                return(cm)
        }
        temp <- A$get()
        cm <- solve(temp, ...)
        A$setInverse(cm)
        cm
} ## Return a matrix that is the inverse of 'A'

