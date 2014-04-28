## 4/27/2014 pjs
## These functions implement a cache based inverse matrix calculation

## creates a matrix and stores it's inverse calculation
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL  
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of a matrix, only calculating if the inverse has not yet been calculated
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){  
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
