#makeCacheMatrix: Creates an object to hold a matrix and its cached inverse
#makeCachedMatrix() will create an empty object (i.e. the matrix needs to be set using makeCachedMatrix$set function)
#makeCachedMatrix(X) will create an object holding the matrix X
makeCacheMatrix <- function(x = matrix()) {
  #The inverse is intially null
  inv <- NULL
  
  #set: sets the matrix values. It also resets the inverse since the data has changed
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get: return matrix
  get <- function() x
  
  #setInv: set the matrix inverse
  setInv <- function(d) inv <<- d
  
  #getInv: return the matrix inverse
  getInv <- function() inv
  
  
  list(set = set, get = get,setInv = setInv, getInv = getInv)
}

#cacheSolve: calculates the inverse function of the matrix object
cacheSolve <- function(x, ...) {
  #Try to get the cached inverse
  inv <- x$getInv()
  
  #If the matrix doesn't have an inverse yet, then calculate it
  if(is.null(inv)) 
    {
    #Get the matrix first
    data <- x$get()
    #Then find the inverse using solve() function
    inv <- solve(data, ...)
    #Finally, cache the value of the inverse in the matrix object 
    x$setInv(inv)
    }
  
  #Return the matrix inverse
  return (inv)
}

#This is just a testing function
Test <- function()
  
{
  print("Intialize Matrix:  M<-makeCacheMatrix(matrix(data=rnorm(9,1),nrow=3,ncol=3,byrow=T))")
  M<-makeCacheMatrix(matrix(data=rnorm(9,1),nrow=3,ncol=3,byrow=T))

  print('Matrix Values M$get()')
  print(M$get())
  
  print('Inverse not set yet M$getInv()')
  print(M$getInv())
  
  print("Calculate Inverse cacheSolve(M)")
  print(cacheSolve(M))
  
  print("The inverse is now cached M$getInv()")
  print(M$getInv())
  
  print("Assign new value values to the matrix: M$set(matrix(data=rnorm(9,1),nrow=3,ncol=3,byrow=T))")
  M$set(matrix(data=rnorm(9,1),nrow=3,ncol=3,byrow=T))
  
  print("The cache inverse is reset to NULL: M$getInv()")
  print(M$getInv())
}