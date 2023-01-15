# PROGRAMMING ASSIGNMENT 2 - LEXICAL SCOPE


## First, we're gonna create a function that will allow us to create a matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Then, we're gonna create a function that will allow us to call the inverse
## of a matrix if it was already computed and stored by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting the inverse of the matrix from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  message("computing the inverse of the matrix - stored in cache")
  inv
}


## Now let's test the functions with an example

matr <- matrix(round(rnorm(25), 3), 5, 5)
### here we are creating a 5x5 matrix. It won't change unless you run it again.
matr

ejem <- makeCacheMatrix(matr)
round(cacheSolve(ejem), 3)
### first it will compute the inverse and store it.
### then it will inmediatly get the inverse from cache

