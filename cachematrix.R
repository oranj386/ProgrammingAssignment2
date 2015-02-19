## The function makeCacheMatrix:
## 1. Sets a given matrix
## 2. Gets the matrix for the current environment
## 3. Sets the inverse of the original matrix
## 4. Gets the inverted matrix for the current environment

## This assumes that the supplied maxtrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list (set=set,
              get=get,
              setinverse=setinverse,
              getinverse=getinverse)
}


## The function cacheSolve:
## 1. Looks for the inverse of a matrix
## 2. If the inverse maxtix is there, it is retrieved
## 3. If inverse matrix is not there:
##### a. Computes the inverse matrix
##### b. Stores the inverse matrix
##### c. Prints the inverse matrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

## Test your code
# source("cachematrix.R")

## 1 generate matrix, and the inverse of the matrix.
# size <- 1000 
# mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
# mymatrix.inverse <- solve(mymatrix)

## 2 Solve the matrix via the cache-method
# special.matrix   <- makeCacheMatrix(mymatrix)

## 3 (long)
# special.solved.1 <- cacheSolve(special.matrix)

## 4 (fast)
# special.solved.2 <- cacheSolve(special.matrix)

## 5 check if all solved matrices are identical
# identical(mymatrix.inverse, special.solved.1) & 
#        identical(mymatrix.inverse, special.solved.2)


