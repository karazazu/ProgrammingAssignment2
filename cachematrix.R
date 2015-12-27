
## The two functions below are for creating a special object that stores a matrix and caches its inverse. The reason for caching is that, matrix operations can be 
## computationally costly, so caching an already computed copy of the inverse could save compute resource; as after initial computation,  it remains iavailable in memory without further re-calculation.

## Write a short comment describing this function
## The first function, makeCacheMatrix,creates a special object, which is really a list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix innverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The second function, cacheSolve, uses the R 'solve' functon to invert a matrix. Before calculating the inverse, it checks to see if a cached copy is available,
## and returns the cached copy of it exists. If the cached copy does not exist, it first computes and caches the inverse before returning the inverse to the calling function.
## NB. the input matrix is assumed to be invertible

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv 

}
