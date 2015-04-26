

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # init default value
  inverse <- NULL 
  #init default value
  y <- NULL
  ##set matrix
  setmatrix <- function(y) { 
    ## cache the  matrix
    x <<- y 
    inverse <<- NULL
  }
  #return the matrix
  getmatrix<-function()x
  #set inverse of the matrix
  setinverse<-function(i)inverse<<-i
  #get inverse
  getinverse<-function()inverse
  # List of 4 functions
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
  
  

}


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedinverse<-x$getinverse()
  #if it is null calculate the inverse
  if(is.null(cachedinverse))
  {
    cachedinverse<-solve(x$getmatrix())
    x$setinverse(cachedinverse)
    
  }
    return(cachedinverse)
}
