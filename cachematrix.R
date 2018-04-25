## Put comments here that give an overall description of what your
## functions do

## As before, this function will create a Cache Matrix and allows you to gain access to the functions set as set and get

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    x <<- matrix()
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This solves for the inverse if it is not inside cache, and simply searches for the solution if it is already inside cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


x = rbind(c(2, 5), c(5, 2))
m = makeCacheMatrix(x)
m$get()


cacheSolve(m)





