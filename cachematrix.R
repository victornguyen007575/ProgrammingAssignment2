## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix created with makeCacheMatrix function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}

m <- makeCacheMatrix(matrix(c(2, 3, 1, 4), nrow = 2))
m
cacheSolve(m)
