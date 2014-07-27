## In this file, following two functins are defined:
## 1. makeCacheMatrix(): This function creates a matrix object that can cache the
## inverse matrix.
## 2. cacheSolve(): This function tries to get inverse of the given matrix from the
## cache, if exists. Otherwise, it computes the inverse of the matrix.

## This function returns an matrix object that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  #will contain inverse matrix
  inv <- NULL
  
  #sets matrix
  setMatrix <- function(y)
  {
    #y should be a matrix
    if(is.matrix(y))
    {
      x <<- y
    }
    else
    {
      x<<-NULL
    }
    
    inv <<- NULL
  }
  
  #returns matrix
  getMatrix <- function()
  {
    x
  }
  
  #sets inverse matrix
  setInverseMatrix <- function(i)
  { 
    #i should be a matrix
    if(is.matrix(i))
    {
      inv <<- i
    }
    else
    {
      inv<<-NULL
    }
  }
  
  #returns inverse matrix
  getInverseMatrix <- function()
  {
    inv
  }
  
  #returns matrix object as a list
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


##This function can retrieve the inverse from cache, if it exsits. Otherwise, it will
##compute it.

cacheSolve <- function(x, ...) 
{
  
  #get inverse from cache      
  inv <- x$getInverseMatrix()
  
  #check whether NULL
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  #get matrix data from cache
  data <- x$getMatrix()
  
  #calculates inverse of the matrix
  inv <- solve(data) %*% data
  
  #adds inverse to the cache
  x$setInverseMatrix(inv)
  
  #returns inverse matrix
  inv     
}

