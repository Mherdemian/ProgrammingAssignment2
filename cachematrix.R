a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
makeCacheMatrix <- function(x = matrix()) { # Creates a special "matrix" object that can cache its inverse
  m <- NULL # Stores inversion result
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # Gets the input matrix
  setmatrix <- function(solve) m <<- solve # Sets inverse matrix
  getmatrix <- function() m # Gets inverse matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
cacheSolve <- function(x=matrix(), ...) { # Computes the inverse of the special "matrix" returned by makeCacheMatrix above
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
cacheSolve(a)