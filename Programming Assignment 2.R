##function 01

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse)inv <<- inverse
        getinverse <- function()inv
        list(set=set, get=get, setinv=setinverse, getinv=getinverse)
}

##function 02

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inverse <- solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}