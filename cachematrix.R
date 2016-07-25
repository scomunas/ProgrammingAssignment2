## Put comments here that give an overall description of what your
## functions do


## This function creates a special matrix that handles space to achieve
## his inverse in a list
makeCacheMatrix <- function(x = matrix()) {
    ## Empty inverse matrix if run empty function
    InverseMatrix <- NULL
    
    ## When run set sub-function set the value and empty Inverse Matrix
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    
    ## Print the current value of the Matrix
    get <- function() x
    
    ## Fill in the Inverse Matrix solution cached
    setinverse <- function(VarInverse) InverseMatrix <<- VarInverse
    
    ## Get the Inverse Matrix solution cached
    getinverse <- function() InverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Fill in the Inverse Matrix solution in the cached variable
cacheSolve <- function(x, ...) {
    ## Look for current Inverse Matrix, if it's filled
    InverseMatrix <- x$getinverse()
    
    ## If there's a solution cached return the actual value
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    
    ## If not get the Initial Matrix, calculate inverse with solve() and stores
    ## it in the global variable
    data <- x$get()
    InverseMatrix <- solve(data, ...)
    x$setinverse(InverseMatrix)
    InverseMatrix
}
