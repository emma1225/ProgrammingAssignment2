## Below are two functions that are used to create a special object that stores a matrix
## and caches its inverse.

## The first function,makeCacheMatrix creats a special "matrix",which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
              x<<-y
              i<<-NULL
        }
        get<-function()x
        setinverse<-function(solve)i<<-solve
        getinverse<-function()i
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)  
}


## The second function calculates the inverse of the special "matrix" mentioned above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i<-x$getinverse()
         if(!is.null(i)){
                 message("getting cached data")
                 return(i)
         }
         data<-x$get()
         i<-solve(data,...)
         x$setinverse(i)
         i
}
