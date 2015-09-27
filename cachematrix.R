makeCacheMatrix <- function(x = matrix()) {
  mtx<-NULL
  set<-function(y){
    x<<-y
    mtx<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<- inverse
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The function calculates the inverse of the special "matrix" created by the function 
## above. 

cacheSolve <- function(x, ...) {
  iv<-x$getinverse()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  mtx <- x$get() 
  iv<-solve(mtx, ...)
  x$setinverse(iv)
  iv
}