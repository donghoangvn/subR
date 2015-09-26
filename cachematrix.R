## The following functions work together to create an invertible matrix
## and make the inverse of the matrix available in the cache environment.

## makeCacheMatrix creates environments
makeCacheMatrix <- function(x = matrix(0,3,3)) {
## set and get the value of the matrix
    cache <- NULL
    set <- function(y) {
      x <<- y
      cache <<- NULL
    }
    get <- function() x
## set and get the value of the inverse
    setinv <- function(inverse) cache <<- inverse
    getinv <- function() cache
## return the created functions to the working environment
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## z<- makeCacheMatrix(x)
## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(z, ...) {
## check the inversed environment
      m<-z$getinv()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
## calculate the inverse
    matr<-z$get()
    m<-solve(matr, ...)
## set the value of the inverse
    z$setinv(m)
    return(m)
}
