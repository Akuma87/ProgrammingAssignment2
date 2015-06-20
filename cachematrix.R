

makeCacheMatrix <- function(x = matrix()) {
  mat<- NULL
  ## setting the value of matrix and its inverse
  set <- function(y) {
  x <<- y
  mat <<- NULL
  }
  
  #getting the value of matrix and its inverse 
  get <- function() x
  setinv <- function(inverse) 
  mat<<- inverse
  getinv <- function() mat
  #creating the list to make use of all the functions
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#The following function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  mat <- x$getinv()
  #checking if inverse matrix is already present 
  if(!is.null(mat)) {
    message("getting cached matrix. No need to compute")
    return(mat)
  }
  #if inverse matrix isnt present, it will be computed 
  data <- x$get()
  mat <- solve(data)
  x$setinv(mat)
  mat # return inverse matrix 
}
