# make a matrix and stored its inverse matrix using cache

makeCacheMatrix <- function (x =  matrix()){
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse<- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

 # if inverse matrix already stored in cache, return this value, if not recalclate inverse matrix              
cacheSolve <-function(x,...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <-solve(data,...)
        x$setinverse(m)
        m
}
