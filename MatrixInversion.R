# The folowing method cahes matrix inversion for the ease of reuse if/when results are available.

makeCacheMatrix <- function( x = matrix() ){
      
      i <- NULL
      
      set <- function (y){
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inverse){
            i <<- inverse
      }
      
      getinverse <- function() i
      
      list( set = set, 
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

# The following method computes the matri inversion of the provided input matrix
# After the inverse id computed the results are cached as defined in the method above
cacheSolve <- function(x, ...){
      
      i <- x$getinverse()
      
      if(!is.null(i)){
            message("Getting cached data")
            return(i)
      }
      
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      
      i
}
