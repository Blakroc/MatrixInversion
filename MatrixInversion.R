################################
# Description:  The folowing method caches the matrix inversion of an input for the ease of reuse,
#               if/when results are available.
# Methods available: Innterally get andset methods are used to get or set the input matrix.
#                    getinverse and setinverse are used to get or set the input matrix's inverse matrix result.
################################
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
################################
# Dsecription: The following method computes the matrix inversion of the provided input matrix
#              After the inverse id computed the results are cached by calling methods from 'makeCacheMatrix'
# Methods available: cacheSolve has no internally defined function, 
#                    when called the method caches results and returns the same.
################################
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
