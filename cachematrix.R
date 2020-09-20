## This two functions can cache the inverse of a matrix

## The first function This function creates a  object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function returns the inverse of the object 
## that was created for the first function 

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
} 
