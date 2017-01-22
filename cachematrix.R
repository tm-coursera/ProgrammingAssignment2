## The functions calculate the inverse of a matrix 
## and store both the matrix and the inverse of a matrix in a list,
## assigned to an object

## The function creates a list of functions and stores the matrix x
## and the inverse of the matrix x
## $set can be used to store matrix  x
## $get can be used to call a stored matrix x
## $setsolve can be used to store the inverse of  matrix x
## $getsolve can be used to call a stored inverse matrix x


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## The function checks if an inverse matrix is stored 
## in the object x created by makeCacheMatrix.
## If so, the function returns the stored inverse matrix .
## If not, the function determines the inverse matrix of the 
## stored matrix from object x and returns the inverse to object x
 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
