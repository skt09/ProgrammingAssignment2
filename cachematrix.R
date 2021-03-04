## The makeCacheMatrix() function creates a special type of matrix which also
## contains the list of functions interacting with the matrix too.
## The functions are
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse matrix
## 4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function takes a matrix created by the previous function
## and checks if that has the inverse matrix if not found it will calculate
## the inverse matrix otherwise it will collect it from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if (!is.null(inverse)) {
          print("getting cached data...")
          return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinv(inverse)
        inverse
}
