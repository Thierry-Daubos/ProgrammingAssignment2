

## The function makeCacheMatrix creates a special "matrix" as a list
## containing functions to
## 1) set : set the value of the matrix
## 2) get : get the value of the matrix
## 3) setinverse : set the value of the inverse
## 4) getinverse : get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL
      # Set fuction changes the matrix stored in the main function
      set <- function(y) {
             x <<- y
             i <<- NULL
      }
      # Returns the vector x stored in the main function (no input needed)
      get <- function() x
     
      # setinverse and getinverse don't calculate the inverse of the matrix x
      # They simply store the value of the input in a variable i into the main
      # function makeCacheMatrix (setinverse) and return it (getinverse)
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      
      # The following line stores the four functions:
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The function cacheSolve calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function above
## It first checks if the inverse has already been calculated.
## -> if it is the case, it gets the inverse from the cache and skips the computation
## -> if not so, it calculates the inverse of the data and sets the value of the
## inverse in the cache using the setinverse function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      
      # To use the functions stored in the main function, we subset the main function
      # Hence the syntax: main_function + "$" + second function + (arguments)
      
      # cacheSolve verify if the value i, stored previously with getinverse exists
      # and is not NULL. If it exists in memory, it simply returns the value i
      # If not data gets the matrix stored with makeCacheMatrix, i calculates
      # the inverse of the matrix and x$setinverse(i) stores it in the object
      # assigned with makeCacheMatrix
      i <- x$getinverse()
      if(!is.null(i)) {
             message("getting cached data")
             return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
