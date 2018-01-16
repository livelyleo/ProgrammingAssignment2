## This file makes use of lexical scoping to cache the inverse of a matrix
## This file is a part of Programming Assignement 2 for R Programming Course

## The below function takes a special "matrix" as input and return a 
## list of elements which can both set and get the values of the matrix, 
## as well the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set<-function(y){
    x<<-y
    invx<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)invx<<-inverse
  getinverse<-function()invx
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The below function calls the special "matrix" as an argument and calculates
## the inverse of the matrix, if not already calculated before and present in the cache

cacheSolve <- function(x, ...) {
  invx<-x$getinverse()
  if(!is.null(invx)){
    message("getting cached data")
    return(invx)
  }
  data<-x$get()
  invx<-solve(data,...)
  x$setinverse(invx)
  invx
}
