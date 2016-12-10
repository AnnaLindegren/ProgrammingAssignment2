## Martixes cache the inverse of already existing matrix, simplyfing the  matrix inversion costly computation


## makeCacheMatrix creates a special "matrix" objects that caches its intevse

makeCacheMatrix <- function(x = matrix()) {
ivt <- NULL
  set <- function(y) {
    x <<- y
    ivt <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
        get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve computes the inverse of the a special "matrix" returned by makeCacheMatrix.  cacheSolve should retrive the inverse from the cache, if the inverse has already been calculated.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}

##testing my functions
AnnaS_matrix <- makeCacheMatrix(matrix(2:6, 2, 2))
AnnaS_matrix$get()

