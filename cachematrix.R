## This assignment will show us how to store objects in the cache. The 
## functions will compute the inverse of a matrix but return the inverse
## from the cache if it has already been calculated and set in the cache.


## This function creates list of functions to set and get objects from the cache.
## The functions and the objects can be accessed from the parent invironment if 
## assigned to an object in the parent environment.

makeCacheMatrix <- function(x = matrix()) 
{  
    inv <- NULL      
    set <- function(y)        ## Set the matrix in the cache
    {
      x <<- y
      inv <<- NULL
    }
    get <- function() x       ## Return the cached matrix
    setinverse <- function(inverse) inv <<- inverse   ## Set the inverse in the cache
    getinverse <- function() inv    ##Return the chached inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse but returns the inverse from the cache
## if it has already been calculated and set in the cache

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv))   ## Check if the inverse has been calculated
    {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()     ## Calculate inverse
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inv
}
