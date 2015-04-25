## The following functions calculate the inverse of a matrix
## and caches the inverse for when it's required again

## makeCacheMatrix caches inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        # set the inv to NULL as a placeholder
        inv <- NULL  
        # define a function to set matrix x to new matrix y
        set <- function(y) {  
                x <<- y
                inv <<- NULL
        }
        # return matrix x
        get <- function() x  
        # set the calculated inv to cached inv
        setinverse <- function(inverse) inv <<- inverse  
        # return the inverse, inv
        getinverse <- function() inv 
        # return the special vector containing all the funcs above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  

}


## cacheSolve calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the inverse of specified matrix
        inv <- x$getinverse()
        # if the inverse is not NULL, means it's cached, return the cached value
        if(!is.null(inv)) {     
                message("getting cached data")
                return(inv)
        }
        # if inverse is NULL, means not cached, get the matrix and calculate inverse
        data <- x$get() 
        inv <- solve(data, ...)
        # set the calculated inv to cached inv
        x$setinverse(inv) 
        inv
}
