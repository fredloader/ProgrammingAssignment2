## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## Example: s <- makeCacheMatrix(matrix5:8,2))  
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {  ## Set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x   ## Get the value of the matrix
        setsolve <- function(solve) m <<- solve  ## Set the value of the inverse
        getsolve <- function() m                 ## Get the value of the inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)


}

## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix.
## This function will retrieve the inverse from the cache if it has already been calculated.
## Example: Invoking cacheSolve(s) for the first time will compute and display the inverse of the matrix.
##          Invoking cacheSolve(s) for the second time will display "getting cached data" and
##          display the cached value of the inverse. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        m <- x$getsolve()						## Let's check the cache if the inverse is already there.
        if(!is.null(m)) {						## If it's already there,
                message("getting cached data")	## display this message on the console
                return(m)						## and return the value
        }
        data <- x$get()							## There's nothing in the cache,
        m <- solve(data, ...)					## So, we compute the inverse of the matrix,
        x$setsolve(m)							## and store the value to the cache
        m
		
}
