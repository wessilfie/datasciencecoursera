## This function takes in a matrix and calculates its inverse
## makeCacheMatrix saves the previous result 


## caches the inversed matrix to make process faster for more intensive cases
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x 
  setInverse <- function(inverse) <<- inverse 
  getInverse <- function() m 
  matrix(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)

}


## Write a short comment describing this function
##computes inverse of the matrix using R's solve function 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting matrix inverse")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    }


