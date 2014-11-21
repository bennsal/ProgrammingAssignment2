## Put comments here that give an overall description of what your
## functions do

        #  Functions to find the inverse of a matrix.  Since this calculation 
        #  may be slow to perform the result is saved in a cache so that it 
        # only need to be calculated once per session if the matrix is not
        # changed.  

## Write a short comment describing this function

        # makeCacheMatrix generates a list of four fuctions that are used to get
        # and set the inverse matrix in the cacheSolve function below

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
        # cacheSolve returns a matrix that is the inverse of x.  Firstly the 
        # function checks for a previously solved inverse and returns it if 
        # present witht the notice "getting cache data",
        # if null is returned instead then the inverse is calculated 
        # saved in a cache and returned.

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
