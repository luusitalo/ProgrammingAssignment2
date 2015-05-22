## This function creates a list object to cache the inverse of an inversible matrix
## (no handling of erroneous input here!)
## If the inverse is calculated already, that value will be used

## use by creating a cache object for youur matrices:
## e.g. aC<-makeCacheMatrix(a)
## When you need the inverse, call cacheSolve(aC)

# THIS FUNCTION:
## Create a list that contains functions to
## set the value of the original matrix
## get the original matrix
## set the inverse matrix
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(z) inv <<- z # will be called in the cacheSolve function
        getInverse <- function() inv
        #setInverse(x) #actually calculate the inverse to go to cache
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Return the inverse matrix
## Checks to see if it's been caclucated & cached already
## if yes, uses that (and gives a message), otherwise computes and caches it

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                inv <- x$getInverse()
                return(inv)
        }
        #
        matrix<-x$get()
        inv<- solve(matrix)
        x$setInverse(inv)
        return(inv)
        
}
        