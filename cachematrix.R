

makeCacheMatrix <- function(x = matrix()) {
  mat<- NULL
  ## setting the value of matrix and its inverse
  set <- function(y) {
  x <<- y
  mat <<- NULL
  }
  
  #getting the value of matrix and its inverse 
  get <- function() x
  setinverse <- function(inverse) 
  mat<<- inverse
  getinverse <- function() mat
  #creating the list to make use of all the functions
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The following function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  #checking if inverse matrix is already present 
  if(!is.null(mat)) {
    message("getting cached matrix. No need to compute")
    return(mat)
  }
  #if inverse matrix isnt present, it will be computed 
  data <- x$get()
  mat <- solve(data)
  x$setinverse(mat)
  mat # return inverse matrix 
}
