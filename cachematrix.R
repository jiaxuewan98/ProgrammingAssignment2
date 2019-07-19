
## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        get<-function()x 
        setsolve<-function(solve)inv<<-solve
        getsolve<-function()inv 
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function

## This function computes the inverse of the matrix created by makeCacheMatrix. 
## If the inverse has already been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
