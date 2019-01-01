## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCachMatrix function can creates a matrix which is a list 
## containing a function to set the matrix, get the matrix, set 
## the inverse of matrix and get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        makeinverse <- function(inverse) m <<- inverse
        getinverse <- function () m
        list(set = set, get = get,
             makeinverse = makeinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## cashSolve compute the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been 
## calculated, then the cashesolve should retrieve the inverse
## from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$makeinverse(m)
        m
}