

## The function makeCacheMatrix creates a special "matrix" as a list
## containing functions to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL
      set <- function(y) {
             x <<- y
             i <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) i <<- inverse #
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The function cacheSolve calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function above
## It first checks if the inverse has already been calculated.
## -> if it is the case, it gets the inverse from the cache and skips the computation
## -> if not so, it calculates the inverse of the data and sets the value of the
## inverse in the cache using the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      i <- x$getinverse()
      if(!is.null(i)) {
             message("getting cached data")
             return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(m)
      i
}
