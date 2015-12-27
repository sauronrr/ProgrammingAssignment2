## Put comments here that give an overall description of what your
## functions do
## This function solve the inverse square matrix it's supouse that the matrix
## is alrady invertible |A|!=0

## Write a short comment describing this function
## maCacheMatrix define the functions to get/set the matrix and get/set 
##the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y)
  {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setsolve <- function(solve) i<<-solve
  getsolve<- function() i
  list(set =set ,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function
## cacheSolve, calculate the inverse matrix with the structure given on makeCacheMatrix
##If no previus inverse was calculated or it's a new matrix use solve function
##if try to calculate the inverse of the same matrix twice return the cache inverse


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minverse<-x$getsolve()
  ##check inverse is already solved to return cached value
  if (!is.null(minverse) )
  {
    message("getting cached data")
  minverse
  }
  ## inverse is not solved jet. it solve the inverse
  else{
    vmatrix<-x$get()
    minverse<-solve(vmatrix)
    x$setsolve(minverse)
    minverse
  }
}
