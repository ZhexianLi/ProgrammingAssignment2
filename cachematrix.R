## Set up inverse matrix cacultion function that store the inversed values.

## Create a matrix function that can both set and get the value of its inverse 

makeCacheMatrix <- function(x = matrix()) {
    ##We are setting the cache file as NULL by default
    cache<-NULL
    ##Set value of the matrix and it should look like the one for function(x)
    set<-function(y){
        x<<-y
        cache<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) cache<<-inverse
    getinverse<-function()cache
    ##return the functions created in the working environment 
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cache<-x$getinverse()
    ##get the value from the cache when is not NULL
    if(!is.null(cache)){
        message("getting cached data")
        retrun(cache)
    }
    data<-x$get()
    ##get the inversed matrix 
    cache<-solve(data,...)
    x$setinverse(cache)
    cache
}
