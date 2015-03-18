## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmat<-function(y){
		x <<- y
		m<<-NULL
	}
	getmat<-function() x
	setinvmat <- function(invmat) m<<- invmat
	getinvmat <- function() m
	
	list(setmat=setmat,getmat=getmat,setinvmat=setinvmat,getinvmat=getinvmat)	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinvmat()
        if(!is.null(m)) {
        	message("Already in cache")
        	return(m)
        }
        data<-x$getmat()
        m<- solve(data)
        x$setinvmat(m)
        m
}
