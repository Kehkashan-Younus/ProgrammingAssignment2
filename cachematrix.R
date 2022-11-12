## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 ## initializing inverse as Null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x        ## function to get matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv    ## function to get inversoe of Matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheinverse <- function(x, ...) {       #gets cache data
  inv <- x$getinverse()
  if(!is.null(inv)) {                    # checking whether inv is NUll
    message("getting cached data")
    return(inv)                         #returns inv value
  }
  
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)   #calculate inv value
  x$setinverse(inv)
  inv
}                                       # returning matrix inv of x


