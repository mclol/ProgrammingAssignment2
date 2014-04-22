## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special matrix object that can 
## cache its inverse. The special matrix object is really a list containing
## functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## holds the cached value of the inverse
        inv <- NULL
        
        ## sets the value of the matrix
        set <- function(y) {
                ## set matrix to new matrix
                x <<- y
                ## set cached inverse to NULL
                inv <<- NULL
        }
        
        ## get the matrix value
        get <- function() x
        
        ## set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        ## get the inverse
        getinverse <- function () inv
        
        ## build the list and return it
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of a special matrix created with the 
## makeCacheMatrix function above. It first checks to see if the inverse of the
## special matrix has already been calculated and if so returns the cached value.
## Otherwise it calculates the inverse of the matrix and sets the value of the
## inverse via the setinverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached inverse")
                return (inv)
        }
        
        ## get the matrix
        m <- x$get()
        
        # calculate inverse
        inv <- solve(m)
        
        x$setinverse(inv)
        
        inv
        
}
