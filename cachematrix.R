## makeCacheMatrix returns a list of functions for 
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse
##  4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 
    invmat <- NULL
    set <- function(y) {
      x <<- y
      invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with the above function 
## 1. Checks whether inverse has already been calculated
## 2. If so, gets the inverse from the cache and skips computation
## 3. Otherwise, calculates the inverse from data and 
## 4. Sets  inverse in  cache via setinverse function


cacheSolve<- function(x, ...) {
  
  invmat <- x$getinverse()
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    data <- x$get()
    invmat <- solve(data, ...)
    x$setinverse(invmat)
    invmat
}

