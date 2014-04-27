## creates a special "matrix", which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## get the value of the inverse of the matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##  function calculates the inverse of the special "matrix" created with the above function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## checks to see if the inverse has already been calculated
  if(!is.null(i)) {
    ##cached - so skipping computation
    message("getting cached data")
    return(i)
  }
  ## not cached - calculates/sets the inverse via the setinverse function
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}