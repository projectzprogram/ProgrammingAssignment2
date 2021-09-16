## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Goals: Create CacheMatrix function that:
#Sets the Value of the matrix
#Retrieves the Value of the matrix
#Sets the inverse of the matrix
#Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {                           #Sets the matrix values
    x <<- y
    i <<- NULL
  }
  get <- function() x                            #returns the matrix
  setinverse <- function(inverse) i <<- inverse  #Sets the inverse that is calculated
  getinverse <- function() i                     #returns the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#Retrieves cached inverse of the matrix or computes it if there is nothing cached
#Returns the inverse
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
