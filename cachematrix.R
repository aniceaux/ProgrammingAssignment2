## Write a pair of functions that cacche the inverse of a matrix


## The makeCacheMatrix function creates a special matrix object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
    ## initialize inverse fro cache
    i <- NULL
    
    ## set value of matrix
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    ## get value of matrix
    get <- function() x
    
    ## set inverse of matrix
    setInverse <- function(inverse) i <<-inverse
    
    ## get inverse of matrix
    getInverse <- function() i
    
    ## return list
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## The cachesSolve function computes the invers of the special matrix
## returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
      ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      
      ## retrieve inverse from cache if the inverse is already calculated
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      
      ## get the matrix
      data <- x$get()
      
      ## calculate the inverse
      i <- inverse(data, ...)
      
      ## set the inverse
      x$setInverse(i)
      
      ## return the matix
      i
}
