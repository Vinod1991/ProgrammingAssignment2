
## This function "makeCacheMatrix" does the basic work of storing & editing data 
## as and when required. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function "CacheSolve" fetches the value from the above function"makeCacheMatrix" if inverse  
## is present or else finds the inverse on its own and returns the value.

cacheSolve <- function(x=matrix(), ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data<- x$get()
    i <-solve(data) 
    x$setinverse(i)
    i
  }
