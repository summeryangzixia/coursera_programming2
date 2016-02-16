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
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
