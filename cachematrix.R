## my first functions do four things 
## 1. sets the value of matrix
## 2. Outputs the value of the matrix
## 3. sets the value of inverse of the inputed matrix
## 4. outputs the inverse of the inputed matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##outputs the matrix
  get <- function() x 
  ##sets the value of inverse to inv
  setinv <- function(inv) m <<- inv 
  ##outputs the value of the inverse
  getinv <- function() m 
  ## final step creates a list of all four functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## my 2nd funtion do two things
## 1st it looks for inverse of the inputed matrix in cache
## if it is already in the cache then it will simply return that value
## if not, then it will compute the inverse using solve function
## and then it will return that value

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Returns a matrix that is the inverse of 'x'
}
