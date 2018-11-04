
## sets the matrix, gets the matrix,

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setimatx <- function(inverse) i <<- inverse
  getimatx <- function() i
  list(set = set, get = get,
       setimatx = setimatx,
       getimatx = getimatx)
}


## cacheSolve checks the matrix, inverts it using the solve() operation and then returns it

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  i <- x$getimatx()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setimatx(i)
  i
}

#test functions

matx <- makeCacheMatrix(matrix(c(5, 6, 7, 9), 2,2))
matx$get()
cacheSolve(matx)

#This only works for a 2x2 matrix, fails with 3x3 matrix
matx2 <- makeCacheMatrix(matrix(1:9, 4,4))
matx2$get()
cacheSolve(matx2)