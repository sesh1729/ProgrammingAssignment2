## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function contains 4 sub functions which allow for setting / getting the values
## of the original matrix, and it's inverse

## cacheSolve function checks if the inverse of a function is already computed, and returns it. 
## If it is not already computed, cacheSolve computes the inverse and also stores it in the cache.


## makeCacheMatrix function contains other functions that allow for the following:
## 1. Setting a value of a matrix. Whenever the "set" function is called, the previous
##    value of the inverse is set to "NULL" to mimic "clearing the cache". 
## 2. Getting the value of the matrix previously set
## 3. Setting a cached value for the inverse of the matrix
## 4. Getting a previously set cached value for the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inversemat <- NULL
    # set the value of the matrix, and delete the old value of the inverse
    set <- function(y){
        x <<- y
        inversemat <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # set the inverse of the matrix in the cache
    setinverse <- function(solve) inversemat <<- solve
    # get the value of inverse stored in the cache
    getinverse <- function() inversemat
    # function always returns the list of outputus of 4 sub-functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function checks if the inverse was previously computed by checking if is not "NULL"
## If the inverse was previously computed, it returns the cached value
## If the inverse was not computed yet, it will first compute the inverse, set it in the cache 
## and then return the same value

cacheSolve <- function(x, ...) {
    # check if inverse is already computed
    inv <- x$getinverse()
    # if the inverse value exists in cache, return that value
    if(!is.null(inv)) {
        # inform that the inverse value is coming from cached-data. 
        message("getting cached data")
        return(inv)
    }
    # if the inverse value doesn't exist cache, get the matrix value and compute the inverse
    # using the built-in "solve" function in R. 
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
