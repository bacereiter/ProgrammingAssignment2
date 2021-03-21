## This assigment uses two functions: makeCacheMatrix, which creates a matrix.
## This matrix has an inverse, and will can cache that inverse if calculated.
## cacheSolve will calculate the inverse of a matrix, but first checks for the
## answer in the cache.

## This assignment uses two functions: makeCacheMatrix, which creates a matrix.
## This matrix has an inverse, and will can cache that inverse if calculated.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL ## Clears the inverseMatrix cache for a new matrix
  set <-function(y){
    x<<- y
    invm <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) invm <<- solve
  getinverse <- function() invm
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve will check the cache for the matrix inverse. If it's there,
## it will print the inverse. If not, it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  data<- x$get()
  invm <-solve(data,...)
  x$setinverse(invm)
  invm
}
## For this code to work properly, I would input:
## aMatrix <- (a matrix that provides an inverse)
## myMatrix <-makeCacheMatrix(aMatrix)
## cacheSolve(myMatrix)
  ## If you re-run cacheSolve(myMatrix), you get "getting cached data" message
  ## before the answer