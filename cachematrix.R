##makeCacheMatrix()<-this function helps cache inverse of a input matrix(the argument),returns a list

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      setm <- function(y) {                ##setm<-helps reset values of a list returned by makeCacheMatrix 
           x<<-y                          
           m <<- NULL
          }
      getm <- function() x                 ##getm<-returns the  input matrix 
      setinv <- function(inv) m <<- inv    ##setinv<-caches the value of inverse of a matrix whenever called  
      getinv <- function() m               ##getinv<-returns the value of inverse of a matrix when called returns NULL if value not assigned    
      list(setm = setm, getm = getm,              
       setinv = setinv,getinv = getinv)
   }

##takes list returned by makeCacheMatrix as argument
cacheSolve <- function(x, ...) {
 m <- s$getinv()                                 ##checks if the inverse of a matrix is present in the cache 
        if(!is.null(m)) {                        ##returns cached value if present
                message("getting cached data")      
                return(m)
         }
         else{
               data <- s$getm()                 ##calculates the value if not present caches it and then returns the inverse
               m <- solve(data, ...)
               s$setinv(m)
        } 
        m                                     ## Return a matrix that is the inverse of 'x'
}

