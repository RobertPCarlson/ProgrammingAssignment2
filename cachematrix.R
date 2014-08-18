## Put comments here that give an overall description of what your
## functions do

#################################################################################
# The two functions 'makeCacheMatrix.R' and 'cacheSolve.R' set up a             #
# system whereby a matrix inverse can be computed only once and then            #
# stored in 'cache' where it can be retrieved and reused without the need       #
# to recompute it, thus saving time if the matrix is large.                     #
#################################################################################

## Write a short comment describing this function: makeCacheMatrix.R

#################################################################################
# 'makeCacheMatrix.R' creates an R-list of four functions that allow the user   #
# to 'set' (assign) and 'get' (retrieve) the value of a matrix (presumed to     #
# be square and invertible) and to 'set' (store) and 'get' (retrieve) the       #
# inverse of this matrix after it has been computed.                            #
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinv <- function(inverse) inv <<- inverse

    getinv <- function() inv

    list( set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function: cacheSolve.R

#################################################################################
# 'cacheSolve.R' returns the inverse of an invertible matrix, computing and     #
# storing it in 'cache' the                                                     #
# first time and retrieving it from 'cache' if it has been computed already.    #
#################################################################################

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinv()

    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()

    inv <- solve(data, ...)

    x$setinv(inv)

    inv

}

#################################################################################
# a sequence of R-statements that use these functions might look like this      #
#>test <- matrix(1:4,2,2)                                                       #
#>x <- makeCacheMatrix()    #sets up the get/set functions associated with x    #
#>x$set(test)               #assigns test to x                                  #
#>x$get()                   #returns test                                       #
#>x$getinv()                #should return NULL the first time                  #
#>cacheSolve(x)             #returns the inverse and stores it in cache         #
#>x$getinv()                #returns the inverse; indicates it came from cache  #
#################################################################################
