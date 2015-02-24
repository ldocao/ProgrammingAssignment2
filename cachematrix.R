## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #flag to check if inverse has been computed
    set <- function(y){
        x <<- y
        inverse <- NULL
    }

    get <- function() x #get the original matrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
