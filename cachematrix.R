##creating a matrix object to cache its own inverse

makeCacheMatrix <- function(a = matrix()) {
    ##Initialization
    i <- NULL
    
    ## Method to set the matrix
    mat <- function( matrix ) {
        a <<- matrix
        i <<- NULL
    }
    
    ##Method to read the above set matrix
    readmat <- function(){
        ## Return the read Matrix
        a
    }
    
    ##Method to set the Inverse matrix
    setInvMat <- function(inverse) {
        i <<- inverse
    }
    
    ##Method to read Inverse matrix
    readInvMat <- function() {
        ##return inverse
        i
    }
    
    ##Forming a list of all the methods created 
    list(mat = mat, readmat = readmat,
         setInvMat = setInvMat,
         readInvMat = readInvMat)
}


## Calulate Inverse of the special matrix by the function makeCacheMatrix().
## If the returned value of inverse is already calculated, then cacheSolve() function
## should retreive the inverse value from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    a <- x$readInvMat()
    
    ## Check if the value is already in cache
    if(!isnull(a)){
        message("Retreiving data from Cache")
        return(a)
    }
    
    ## Reading matrix object using method readmat()
    result <- x$readmat()
    
    ## Calculate the inverse using matrix multiplication
    a <- solve(result) %*% result
    
    ## Set the inverse to the object using method setInvMat()
    x$setInvMat(a)
    
    ## Return the matrix
    a
}
