## makeCacheMatrix: This function creates a special matrix object that can cache its inverse
##cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix .

## creates a special matrix object that can cache its inverse

makeCacheMatrix<- function(a = numeric()) {
  mat <- NULL
  set <- function(b) {
    a <<- b
    mat <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) mat <<- inverse
  getInverse<- function() mat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## computes the inverse of the matrix


cacheSolve <- function(a, ...) {
  mat <- a$getInverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  mat_data <- a$get()
  mat <- solve(mat_data, ...)
  a$setInverse(mat)
  mat
}
