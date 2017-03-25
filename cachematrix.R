## These pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # x argument is an invertible square matrix
        # returns a list used as the input for cacheSolve function by doing 
        # the following  
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse of the matrix
        # 4. get the inverse of the matrix
        
        inv <- NULL
        set <- function(y) {
                # `<<-` operator assign a value to an object in an environment 
                # that is different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # x argument is the output of makeCacheMatrix
        
        inv <- x$getinv()
        
        # if the inverse has already been calculated
        # get it from the cache and skip the computation
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # calculate the inverse 
        matx.data <- x$get()
        inv <- solve(matx.data, ...)
        
        # set the value of the inverse in the cache with the setinv function.
        x$setinv(inv)
        inv
}