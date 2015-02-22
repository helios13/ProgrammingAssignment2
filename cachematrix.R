## makeCacheMatrix - creates a special matrix that can cache its inverse
## cacheSolve - computes the inverse of the special matrix returned by the above function


## "makeCacheMatrix" function is really a list containing function to 
## set the value of matrix, get the value of matrix,
## set the value of the inverse matrix, get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## "cacheSolve" function calculates the inverse of the matrix
## and puts it in the cache after checking whether the inverse is already 
## present in the cache or not 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
