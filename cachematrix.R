## Belowing two of my function work as caching the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The second function, cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated, then it should retrieve the inverse from the cacha.

cacheSolve <- function(x, ...) {
        #Check the inverse of x is in cache
        m<-x$getinverse()
        #If the inverse has already been calculated, retrieve the inverse from the cache.
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        #Get data component of x
        data<-x$get
        #Use solve function to calculate the inverse 
        m<-solve(data,...)
        #Set the inverse in cache
        x$setinverse(m)
        #Display the inverse
        m
}
