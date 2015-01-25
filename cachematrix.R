## Purpose: These 2 functions are complementry in there use.  When invoked
## during the creation of a matrix, 'makeCacheMatrix' creates an environment
## separate from Global_Environment where the matrix and a placeholder for it's
## inverse exists.  In addition, 4 functions are created to allow insertion and 
## retrieval of the matrix and it's inverse.

## The 'cacheSolve' funciton does the work of actually calculating the inverse
## matrix.  When called using the name of a matrix created using 
## 'makeCacheMatrix', 'cacheSolve' looks to so if the inverse has been 
## previously calculated and stored in the environment created in 
## 'makeCacheMatrix' and retrieves it if it has.  Otherwise it calculates the 
## inverse, stores it in 'makeCacheMatrix'

## 'makeCacheMatrix' creates an environment when called to assign a name to a
## matrix.  The environment persists so long as the named matrix exists. It
##  also creates an object called inv.mat and assigns it the NULL value.  The
##  environment includes 4 additional functions: set, get, setinv and getinv.
##    set: inserts a matrix into the environment, creates an objected called 
##          inv.mat and assigns it the NULL value.
##    get: retrieves the matix from the environment.
##    setinv: inserts a matrix into the environment, intended to be the inverse
##          matrix.
##    getinv: retrieves a matrix from the environment, intended to be the
##          inverse matix.

makeCacheMatrix <- function(x.mat = matrix()) {
      inv.mat <- NULL
      set <- function(y) {
            x.mat <<- y
            inv.mat <<- NULL
      }
      get <- function() x.mat
      setinv <- function(solve.mat) inv.mat <<- solve.mat
      getinv <- function() inv.mat
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## When called using the same name used to create an environment with
## 'makeCacheMatrix', 'cacheSolve' uses the 'makeCacheMatrix' getinv function
## to retrieve the matrix stored there.  
## (1) If the returned value is NULL, the inverse has not yet been calculated.
## In that case the 'makeCacheMatrix' get function retrieves the original
## matrix, the R solve function is used to calculate the inverse function, the
## 'makeCacheMatrix' setinv funciton is used to store it the 'makeCacheMatrix'
## environment, and the inverse matrix is output.
## (2) If the returned value is not NULL, the inverse has previously been
## calculated and stored in the 'makeCacheMatrix' environment.  In that case a
## message is printed to to signal the retrieval and the inverse matrix is
## output.

cacheSolve <- function(x, ...) {
      inv.mat <- x$getinv()
      if(!is.null(inv.mat)) {
            message("getting cached data")
            return(inv.mat)
      }
      data <- x$get()
      inv.mat <- solve(data, ...)
      x$setinv(inv.mat)
      inv.mat
}

