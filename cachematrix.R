
## makeCacheMatrix creates a matrix and caches its inverse.
## cacheSolve compute the inverse and retreive it from the cache.

## makeCacheMatrix creates a square matrix object that can cache its inverse. 
makeCacheMatrix <- function(X = matrix()) {
  ## x: an invertible matrix 
  inverse <- NULL
  ## set the matrix
  set <- function(Y){
    # `<<-` to assign a value to the matrix in an environment
    X <<- Y
    inverse <<- NULL
  }
  ## get the matrix
  get <- function() X
  ## set the inverse
  setinverse <- function(Inverse) inverse <<- Inverse
  ## get the inverse
  getinverse <- function() inverse
  ## this below list is used as the input to cacheSolve()
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix, then retrieve its inverse form the cache.
## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition
cacheSolve <- function(X, ...) 
{ ## x: output of makeCacheMatrix()
  if(require("corpcor")){
    print("corpcor is properly loaded")
  } else {
    print("Intalling corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("corpcor not properly installed")
    }
  }
  inverse <- X$getinverse()
  
  ## if the inverse has already been calculated
  if(!is.null(inverse)){
    # get it from the cache and skips the computation. 
    message("matrix exists")
    return(inverse)
  }
  # otherwise, calculates the inverse 
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  
  ## sets the value of the inverse in the cache via the setinv function.
  X$setinverse(inverse)
  ## display inverse of the original matrix input to makeCacheMatrix(
  return(inverse)
}

