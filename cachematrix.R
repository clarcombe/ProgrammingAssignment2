## Creation of an object to store a matrix which has been created locally
## Test with the following commands

## Create a 5,5 random matrix
## mymatrix<-matrix(sample.int(15, 9*100, TRUE),ncol=5,nrow=5)

## Instantiate the object and pass the 5,5 matrix to initialise it
## mymatrixobject <- makeCacheMatrix(mymatrix)

## Now run the inverse function on the instantiated object
## cacheSolve(mymatrixobject)
## The inverse has now been created and can be viewed with
## mymatrixobject$getinverse

## Run the CacheSolve again, this time as the matrix has already been inversed
## it will just indicate the cached value will be used and not recalculated

makeCacheMatrix <- function(x = matrix()) {

        matrixvalue <- NULL
        
        # Method to initialise the matrix which was passed as a parameter
        set <- function(y) {
                x <<- y
                matrixvalue  <<- NULL
        }
        
        # Method to return the matrix 
        get <- function() x
        
        # Define the methods for setting and retrieving the inverse
        setinverse <- function(solve) matrixvalue <<- solve
        getinverse <- function() matrixvalue
        
        # Show all matrices
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve allows a matrix contained in an instantiated object to be inversed
## If the operation has already been performed on the given object
## then the inverse function is not re-run

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}


