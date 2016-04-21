#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  u<-NULL
  set<-function(y){
    x<<-y
    u<<-NULL
  }
  get<-function()x
  setsolve<-function(solve)u<<-solve
  getsolve<-function()u
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}

#This function computes the inverse of a specaial "matrix" 
# returned by a makeCacheMatrix.
#If the inverse has already been calculated(and matrix not changed)
#cacheSolve retrieve the inverse from the cache.
cacheSolve<-function(x, ...){
  u<-x$getsolve()
  if(!is.null(u)){
    message("getting cached data")
    return(u)
  }
  data<-x$get()
  u<-solve(data,...)
  x$setsolve(u)
  u
}