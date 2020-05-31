## The following function calculates a special "matrix" object 
## that can cache its inverse.
## It is really a list containing 4 functions, namely
## 1) function 'set' which sets the value of the matrix
## 2) function 'get' which gets the value of the matrix
## 3) function 'setsolve' which sets the value of the inverse of the matrix
## 4) function 'getsolve' which gets the value of the inverse of the matrix

makeMatrix <- function(x = matrix()) {
        s <- NULL   
        set <- function(y) {
          x <<- y
          s <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}
        
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         
         s <- x$getsolve()
         if(!is.null(s)) {
                 message("getting cached matrix data")
                 return(s)
         }
         data <- x$get()
         s <- solve(data,...)
         
         x$setsolve(s)
         s
}