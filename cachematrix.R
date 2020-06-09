## makeCacheMatrix creates a special matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
    set<-matrix(y){
      x<<-y
      m<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse)m <<-inverse
    getInverse<-function()m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
  	if(!is.null(m)){
    	message("getting cached data")
    	return(m)
  	}
  	mat<-x$get()
  	m<-solve(data,...)
  	x$setInverse(m)
  	m
}
