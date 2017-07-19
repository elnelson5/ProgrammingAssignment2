##Objective: Catch mean of matrix

## Steps: Set value of the matrix
##        Get the value of the inverse matrix
##        Set the value of the inverse matrix
##        Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {         ##initialize object for x
  inv <- NULL                       ## initialize object for inv to be used later in function environment
  get <- function() x               ##Defines getter for matrix x
  set <- function(y) {              ##Defines setter for y
    x <<- y       ## Use assign operator to assign y to x
    m <<- NULL    ## Assign NULL to inv object in parent environment to clear prior cache
    
  }
  
  ## getter and setter for inv
  
  getinv <- function() inv        ##Defines getter for inv
  setinv <- function(inverse) inv <<- inverse   ## defines setter for inv
 
  ## Name the list elements to give name 'set' to set function above, 'get' to get function above, and so on.
  
   list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x,...){
  inv <- x$getinv() 
  
  ##The above and below gets a matrix inverse if we already calculated it
  
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  
  #if we don't already have it from above, compute inverse of matrix
  mat <- x$get()
  inv <- solve(mat,...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
  
}