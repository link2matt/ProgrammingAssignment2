## this script is for the 2nd programming assignment for the programming r class
## these functions demonstrate the use of the superassignment operator
## these functions demonstrate an example of cacheing a copmputation

## the makeCacheMatrix function caches in a list a data matrix and its inverse
## the set function sets the value of the vector
## the get function gets the value of the vector
## the setinverse function sets the value of the inverse
## the getinverse function gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_parent) inverse_matrix <<- inverse_parent
        getinverse <- function() inverse_matrix
        list(set = set, get = get, setinverse=setinverse, getinverse =getinverse)
        
}


## the cacheSolve function modifies the list to either retrieve the inverse of a matrix
## or to calculate the inverse of a matrix if it hasn't already been calculated
## this function first checks whether the inverse has already been calculated
## if the inverse has been calculate already, then computation is skipped
## if the inverse hasn't been calculated, the inverse if computed
## after the calculation, the inverse is cached in the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
        
}

