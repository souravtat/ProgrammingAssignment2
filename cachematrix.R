
## makeCacheMatrix creates R objects that stores a matrix and its inverse
#This function also does a validation check to see if any square matrix has been passed. If not it throws an error.

#######################################
#Sample Call and output  
#  x <- matrix(c(4,2,7,6),ncol=2
#  mymat <- makeCacheMatrix(x
#cacheSolve(mymat)  --Calculates the inverse
#cacheSolve(mymat)  --returns from the cache
#y <- matrix(c(1,2,3,4,5,6),nrow=2) 
#  mymat$set(y)  --Throws error as not square matrix has been passed
#z <- matrix(c(3,6,2,9),nrow=2) 
#mymat$set(z)
#cacheSolve(mymat) - Recalculate the inverse
#############################################################

makeCacheMatrix <- function(x = matrix()) {

  if (nrow(x)!=ncol(x))
  {
  stop("Exiting..Please input a square matrix i.e no of rows=no of columns")
  }
  
   m <- NULL
   set <- function(y){
   if (nrow(y)!=ncol(y))    { stop("Exiting..Please input a square matrix i.e no of rows=no of columns")  }
   x <<- y
   m <<- NULL
   }
     get <- function() x
     setinvmat <- function(inv) m <<- inv
     getinvmat <- function() m
     list(set = set, get = get,
          setinvmat = setinvmat,
          getinvmat = getinvmat)
}


## cacheSolve tries to find the cache value stored in makeCacheMatrix object. If not calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             m <- x$getinvmat()
	     if(!is.null(m)) {
	         message("getting cached data")
	         return(m)
	     }
	     data <- x$get()
	     m <- solve(data, ...)
	     x$setinvmat(m)
             m
}
