## These functions create and maintain a special matrix that is able to 
## cache its inverse for immediate retrieval after the first calculation

## makeCacheMatrix creates a matrix which is able to cache its inverse, 
## along with the functions to set, get and calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<-NULL
      }
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The function will return the pre-calculated inverse of x or calculate
## the inverse of x if not already done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)) {
            message("returning cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      message("returning calculated data")
      i
}
