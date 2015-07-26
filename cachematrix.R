## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Carmen Rodriguez
##cour1234

setwd("c:/coursera/")

#Validate:
#b<-makeCacheMarix()
#b$set(matrix(c(4,2,5,6),2,2))
#b$get()
#chacheSolve(b)


# The following function prepare the matrix

# Set the value of the matrix
# Get the value of the matrix
# Set the value of inverse of the matrix
# Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

#Results:

# b$set(matrix(c(4,2,5,6),2,2))
# b$get()
#     [,1] [,2]
#[1,]    4    5
#[2,]    2    6
# cacheSolve(b)
#           [,1]       [,2]
#[1,]  0.4285714 -0.3571429
#[2,] -0.1428571  0.2857143
 
