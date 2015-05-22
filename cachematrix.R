## This function caches the inverse of an inversible matrix
## (no handling of erroneous input here!)
## It caches the inverse and uses that if the function is called again

## Use by calling cacheSolve(your_matrix)

## Create a list that contain the original data and the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## Return a matrix that is the inverse of 'x'
## Checks to see if there is a cached version, uses it if there is, 
## otherwise computes and caches it

cacheSolve <- function(x, ...) {
        
        z<-makeCacheMatrix(x)
        
        inv <- z$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        matrix <- z$get()
        inv <- solve(matrix)
        z$setInverse(inv)
        inv    
}
        