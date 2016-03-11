## The two functions below together calculate the inverse  
## of a matrix and cache it. Once the inverse is calculated 
## subsequent calls to get inverse will return the cached matrix

## This function creates a list containing functions to
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse of the matrix
## Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	   invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)}


## Calculates the inverse of the matrix created with the above function.
## If inverse has already been calculated returns the cached copy.
## IF not calculates the inverse with the solve() function and sets the 
## value of the mean using the set inverse function. 
## example usage : source("cachematrix.R")
## mymatrix = makeCacheMatrix(matrix(c(4,2,7,6), nrow=2, ncol=2))
## cacheSolve(mymatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        mat <- x$get()
        invmat <- solve(mat, ...)
        x$setinverse(invmat)
        invmat
}