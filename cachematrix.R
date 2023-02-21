## Put comments here that give an overall description of what your
## functions 
## there are two functions, makeCachMatrix and CacheSlove
## makeCacheMatrix is comprised of set,get,setinv,getinv
## library(MASS) is used to calculate theinverse for none squared and squared matrices
 library(MASS)
 
makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL                 ## initializing inverse as a NULL
     set<-function(y){
                  x<<-y
                  inv<<-NULL
   
             }
    get<-function()x                ##function to receive matrix x
    setinv<-function(inverse)inv<<-inverse
    getinv<-function(){
                   inver<-ginv(x)
                   inver%*%x         ##function to gain inverse of a matrix
                  }
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)
}


## Write a short comment describing this function
#this is used for getting cache data


cacheSolve <- function(x, ...)  ## function used to get cache data
  {
   inv<-x$getinv()
   if(!is.null(inv)) {            ##checking whether the inverse is NULL
                  message("getting cache data")
                   return(inv)                  ##returns inverse value
     
   }
   data<-x$get()
   inv<-solve(data,..)                        ##calculate inverse value
   x$setinv(inv)
    inv                   ## Return a matrix that is the inverse of 'x'
}
