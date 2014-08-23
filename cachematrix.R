## The following two functions makeCacheMatrix and CacheSolve
## are used to cache and create the inverse of a matrix

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set <- function(y){
      x<<-y
      i<<-NULL
    }
    get <-function() x
    setInverse<-function(inverse) i<<-inverse 
    getInverse<-function() i
    list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getInverse()
        if(!is.null(i)) {
            message("Getting cached data")
            return(i)
        }
        d<-x$get()
        i<-solve(d)
        x$setInverse(i)
        i
}
