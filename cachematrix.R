
#makeCacheMatrix is used for initializing all the data from matrix


makeCacheMatrix<- function(x = matrix()) {
  INV_xmat<- NULL
  set <- function(y) {
    x <<- y
    INV_xmat<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) INV_xmat<<- inverse
  getinverse <- function() INV_xmat
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
  ##conclusion- variable stored as cache are; 
  #x$set-- initialize the variable
  #x$get-- store the original data
  #x$setinverse -- set the value to inverse
  #x$getinverse -- the inversed data from INV_xmat which will be used in "cacheSolve" 
  #               if the matrix was already inversed
}


cacheSolve <- function(x, ...) {
  INV_xmat<- x$getinverse()
  if(!is.null(INV_xmat)) { 
    message("getting cached data") ##check whether matrix is already inversed
    return(INV_xmat)
  }
  data <- x$get()
  INV_xmat<- solve(data, ...)
  x$setinverse(INV_xmat)
  INV_xmat #return inversed matrix
}