## The following two functions will calculate the inverse matrix or retrieve the inverse 
## matrix from a cache given a invertible matrix, 

## makeCacheMatrix function  returns a list of functions
## used by cacheSolve to get or set an inverted matrix in a cache environment
## @x: a square invertible matrix
## return: a list containing functions to
## a. set the matrix
## b. get the matrix
## c. set the inverse matrix 
## d. get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        
        ## initialize to NULL
        im <- NULL
        
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setim <- function(inverse)  im <<- inverse 
        getim <- function() im
        
        ## return the created functions
        list(set=set, get=get, setim=setim, getim=getim)
        
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix 
## cachesolve  retrieve the inverse from the cache if the inverse has already been calculated (and the matrix has not changed).
## @x: output of makeCacheMatrix
## return: inverse of the original matrix input to function makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        ## Get the inverse of the matrix stored in the cache if it exists
        im <- x$getim()
        
        ## check if the inverse of the matrix was returned
        if (!is.null(im)){
                ## return the inverse of the matrix 
                message("getting cached data")
                return(im)
        }
        
        ## else, compute the inverse of a square matrix using the solve function
        m <- x$get()
        # solve function solves the equation a %*% x = b for x, where b can be either a vector or a matrix
        im <- solve(m, ...)
        
        # set the value of the inverse in the cache 
        x$setim(im)
        
        return(im)
        
}
