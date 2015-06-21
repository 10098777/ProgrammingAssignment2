## Two functions are generated to cache the inverse of a matrix so that 
## the time-consumming computations of the inverse of the same matrix is
## circumvented by directly getting the result from the cache

## makeCacheMatrix function creates a special "vector" which is really a 
## list containing functions to set the value of the vector, get the value
## of the vector,set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the special "vector"
## with the makeCacheMatrix function. However, it first checks whether
## the inverse has already been calculated. If so, it gets the mean from
## the cache. Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}
