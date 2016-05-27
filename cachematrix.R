# R Programming Week 3 Assignment
# Write 2 functions: first which creates an object which can cache matrices and their inverses
# second which can evaluate if the matrix is the same or not and whether to pull the cached inverse

# Matrix Equality Code: Credit to https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y)
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

## Create Cache Matrix class with the ability to save the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Set inv as null to initialize
    set <- function(y) { # Set method which sets new matrix to cache
        x <<- y
        inv <<- NULL
    }
    get <- function() x # Get the matrix
    setinv <- function(invs) inv <<- invs # Set the presented inverse in cache
    getinv <- function() inv # Obtain cached inverse
    list(set = set, get = get, #Create object
         setinv = setinv,
         getinv = getinv)
}


## Compares if the new matrix is the same as the old matrix x
# if so then see if the inverse is in cache, if no, calculate inverse
# if it is in the cache, pull it from cache

cacheSolve <- function(newmat, x, ...){
    origmat <- x$get()
    inv <- x$getinv()
    if (matequal(newmat,origmat) & !is.null(inv)) {
        # Pull From Cache since it exists
        message("getting cached data")
        return (x$getinv())
    } else if (matequal(newmat,origmat) & is.null(inv)) {
        # Calculate inverse since it does not exist
        message("Calculating inverse")
        invers <- solve(origmat)
        x$setinv(invers)
        return (invers)
    } else {
        # Calculate inverse since the matrix is different from before
        message("Calculating inverse")
        x$set(newmat)
        invers <- solve(newmat, ...)
        x$setinv(invers)
        return (invers)
    }
}
